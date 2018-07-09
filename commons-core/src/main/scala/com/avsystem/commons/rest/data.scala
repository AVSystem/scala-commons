package com.avsystem.commons
package rest

import com.avsystem.commons.misc.{AbstractValueEnum, AbstractValueEnumCompanion, EnumCtx}
import com.avsystem.commons.rpc._
import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization.json.{JsonReader, JsonStringInput, JsonStringOutput}
import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec}

import scala.util.control.NoStackTrace

sealed trait RestValue extends Any {
  def value: String
}

/**
  * Value used as encoding of [[Path]] parameters. Types that have `GenKeyCodec` instance have automatic encoding
  * to [[PathValue]].
  */
case class PathValue(value: String) extends AnyVal with RestValue
object PathValue {
  def split(path: String): List[PathValue] =
    path.split("/").iterator.filter(_.nonEmpty).map(PathValue(_)).toList
}

/**
  * Value used as encoding of [[Header]] parameters. Types that have `GenKeyCodec` instance have automatic encoding
  * to [[HeaderValue]].
  */
case class HeaderValue(value: String) extends AnyVal with RestValue

/**
  * Value used as encoding of [[Query]] parameters. Types that have `GenKeyCodec` instance have automatic encoding
  * to [[QueryValue]].
  */
case class QueryValue(value: String) extends AnyVal with RestValue

/**
  * Value used as encoding of [[JsonBodyParam]] parameters. Types that have `GenCodec` instance have automatic encoding
  * to [[JsonValue]].
  */
case class JsonValue(value: String) extends AnyVal with RestValue

object RestValue {
  implicit def pathValueDefaultAsRealRaw[T: GenKeyCodec]: AsRawReal[PathValue, T] =
    AsRawReal.create(v => PathValue(GenKeyCodec.write[T](v)), v => GenKeyCodec.read[T](v.value))
  implicit def headerValueDefaultAsRealRaw[T: GenKeyCodec]: AsRawReal[HeaderValue, T] =
    AsRawReal.create(v => HeaderValue(GenKeyCodec.write[T](v)), v => GenKeyCodec.read[T](v.value))
  implicit def queryValueDefaultAsRealRaw[T: GenKeyCodec]: AsRawReal[QueryValue, T] =
    AsRawReal.create(v => QueryValue(GenKeyCodec.write[T](v)), v => GenKeyCodec.read[T](v.value))
  implicit def jsonValueDefaultAsRealRaw[T: GenCodec]: AsRawReal[JsonValue, T] =
    AsRawReal.create(v => JsonValue(JsonStringOutput.write[T](v)), v => JsonStringInput.read[T](v.value))
}

/**
  * Value used to represent HTTP body. Also used as direct encoding of [[Body]] parameters. Types that have
  * encoding to [[JsonValue]] (e.g. types that have `GenCodec` instance) automatically have encoding to [[HttpBody]]
  * which uses application/json MIME type. There is also a specialized encoding provided for `Unit` which maps it
  * to empty HTTP body instead of JSON containing "null".
  *
  * @param content  raw HTTP body content
  * @param mimeType MIME type, i.e. HTTP `Content-Type` without charset specified
  */
case class HttpBody(content: String, mimeType: String) {
  def jsonValue: JsonValue = mimeType match {
    case HttpBody.JsonType => JsonValue(content)
    case _ => throw new ReadFailure(s"Expected application/json type, got $mimeType")
  }
}
object HttpBody {
  final val PlainType = "text/plain"
  final val JsonType = "application/json"

  def plain(value: String): HttpBody = HttpBody(value, PlainType)
  def json(json: JsonValue): HttpBody = HttpBody(json.value, JsonType)

  final val Empty: HttpBody = HttpBody.plain("")

  def createJsonBody(fields: NamedParams[JsonValue]): HttpBody =
    if (fields.isEmpty) HttpBody.Empty else {
      val sb = new JStringBuilder
      val oo = new JsonStringOutput(sb).writeObject()
      fields.foreach {
        case (key, JsonValue(json)) =>
          oo.writeField(key).writeRawJson(json)
      }
      oo.finish()
      HttpBody.json(JsonValue(sb.toString))
    }

  def parseJsonBody(body: HttpBody): NamedParams[JsonValue] =
    if (body.content.isEmpty) NamedParams.empty else {
      val oi = new JsonStringInput(new JsonReader(body.jsonValue.value)).readObject()
      val builder = NamedParams.newBuilder[JsonValue]
      while (oi.hasNext) {
        val fi = oi.nextField()
        builder += ((fi.fieldName, JsonValue(fi.readRawJson())))
      }
      builder.result()
    }

  implicit val emptyBodyForUnit: AsRawReal[HttpBody, Unit] =
    AsRawReal.create(_ => HttpBody.Empty, _ => ())
  implicit def httpBodyJsonAsRaw[T](implicit jsonAsRaw: AsRaw[JsonValue, T]): AsRaw[HttpBody, T] =
    AsRaw.create(v => HttpBody.json(jsonAsRaw.asRaw(v)))
  implicit def httpBodyJsonAsReal[T](implicit jsonAsReal: AsReal[JsonValue, T]): AsReal[HttpBody, T] =
    AsReal.create(v => jsonAsReal.asReal(v.jsonValue))
}

/**
  * Enum representing HTTP methods.
  */
final class HttpMethod(implicit enumCtx: EnumCtx) extends AbstractValueEnum
object HttpMethod extends AbstractValueEnumCompanion[HttpMethod] {
  final val GET, PUT, POST, PATCH, DELETE: Value = new HttpMethod
}

case class RestHeaders(
  @multi @tagged[Path] path: List[PathValue],
  @multi @tagged[Header] headers: NamedParams[HeaderValue],
  @multi @tagged[Query] query: NamedParams[QueryValue]
) {
  def append(method: RestMethodMetadata[_], otherHeaders: RestHeaders): RestHeaders = RestHeaders(
    path ::: method.applyPathParams(otherHeaders.path),
    headers ++ otherHeaders.headers,
    query ++ otherHeaders.query
  )
}
object RestHeaders {
  final val Empty = RestHeaders(Nil, NamedParams.empty, NamedParams.empty)
}

case class HttpErrorException(code: Int, payload: String)
  extends RuntimeException(s"$code: $payload") with NoStackTrace

case class RestRequest(method: HttpMethod, headers: RestHeaders, body: HttpBody)
case class RestResponse(code: Int, body: HttpBody)
object RestResponse {
  implicit def defaultFutureAsRaw[T](implicit bodyAsRaw: AsRaw[HttpBody, T]): AsRaw[Future[RestResponse], Future[T]] =
    AsRaw.create(_.transformNow {
      case Success(v) => Success(RestResponse(200, bodyAsRaw.asRaw(v)))
      case Failure(HttpErrorException(code, payload)) => Success(RestResponse(code, HttpBody.plain(payload)))
      case Failure(cause) => Failure(cause)
    })
  implicit def defaultFutureAsReal[T](implicit bodyAsReal: AsReal[HttpBody, T]): AsReal[Future[RestResponse], Future[T]] =
    AsReal.create(_.mapNow {
      case RestResponse(200, body) => bodyAsReal.asReal(body)
      case RestResponse(code, body) => throw HttpErrorException(code, body.content)
    })
}
