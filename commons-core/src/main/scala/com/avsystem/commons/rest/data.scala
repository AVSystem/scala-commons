package com.avsystem.commons
package rest

import com.avsystem.commons.misc.{AbstractValueEnum, AbstractValueEnumCompanion, EnumCtx}
import com.avsystem.commons.rpc._
import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization.json.{JsonReader, JsonStringInput, JsonStringOutput}

import scala.util.control.NoStackTrace

sealed trait RestValue extends Any {
  def value: String
}

/**
  * Value used as encoding of [[Path]] parameters.
  */
case class PathValue(value: String) extends AnyVal with RestValue
object PathValue {
  def split(path: String): List[PathValue] =
    path.split("/").iterator.filter(_.nonEmpty).map(PathValue(_)).toList
}

/**
  * Value used as encoding of [[Header]] parameters.
  */
case class HeaderValue(value: String) extends AnyVal with RestValue

/**
  * Value used as encoding of [[Query]] parameters.
  */
case class QueryValue(value: String) extends AnyVal with RestValue

/**
  * Value used as encoding of [[JsonBodyParam]] parameters.
  */
case class JsonValue(value: String) extends AnyVal with RestValue

/**
  * Value used to represent HTTP body. Also used as direct encoding of [[Body]] parameters. Types that have
  * encoding to [[JsonValue]] automatically have encoding to [[HttpBody]] which uses application/json MIME type.
  * There is also a specialized encoding provided for `Unit` which returns empty HTTP body when writing and ignores
  * the body when reading.
  */
sealed trait HttpBody {
  def contentOpt: Opt[String] = this match {
    case HttpBody(content, _) => Opt(content)
    case HttpBody.Empty => Opt.Empty
  }

  def forNonEmpty(consumer: (String, String) => Unit): Unit = this match {
    case HttpBody(content, mimeType) => consumer(content, mimeType)
    case HttpBody.Empty =>
  }

  def readContent(): String = this match {
    case HttpBody(content, _) => content
    case HttpBody.Empty => throw new ReadFailure("Expected non-empty body")
  }

  def readJson(): JsonValue = this match {
    case HttpBody(content, HttpBody.JsonType) => JsonValue(content)
    case HttpBody(_, mimeType) =>
      throw new ReadFailure(s"Expected body with application/json type, got $mimeType")
    case HttpBody.Empty =>
      throw new ReadFailure("Expected body with application/json type, got empty body")
  }
}
object HttpBody {
  object Empty extends HttpBody
  final case class NonEmpty(content: String, mimeType: String) extends HttpBody

  def empty: HttpBody = Empty

  def apply(content: String, mimeType: String): HttpBody =
    NonEmpty(content, mimeType)

  def unapply(body: HttpBody): Opt[(String, String)] = body match {
    case Empty => Opt.Empty
    case NonEmpty(content, mimeType) => Opt((content, mimeType))
  }

  final val PlainType = "text/plain"
  final val JsonType = "application/json"

  def plain(value: String): HttpBody = HttpBody(value, PlainType)
  def json(json: JsonValue): HttpBody = HttpBody(json.value, JsonType)

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

  def parseJsonBody(body: HttpBody): NamedParams[JsonValue] = body match {
    case HttpBody.Empty => NamedParams.empty
    case _ =>
      val oi = new JsonStringInput(new JsonReader(body.readJson().value)).readObject()
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
    AsReal.create(v => jsonAsReal.asReal(v.readJson()))
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

case class HttpErrorException(code: Int, payload: OptArg[String] = OptArg.Empty)
  extends RuntimeException(s"HTTP ERROR $code${payload.fold("")(p => s": $p")}") with NoStackTrace

case class RestRequest(method: HttpMethod, headers: RestHeaders, body: HttpBody)
case class RestResponse(code: Int, body: HttpBody)
object RestResponse {
  implicit def defaultFutureAsRaw[T](implicit bodyAsRaw: AsRaw[HttpBody, T]): AsRaw[Future[RestResponse], Future[T]] =
    AsRaw.create(_.transformNow {
      case Success(v) =>
        Success(RestResponse(200, bodyAsRaw.asRaw(v)))
      case Failure(HttpErrorException(code, payload)) =>
        Success(RestResponse(code, payload.fold(HttpBody.empty)(HttpBody.plain)))
      case Failure(cause) => Failure(cause)
    })
  implicit def defaultFutureAsReal[T](implicit bodyAsReal: AsReal[HttpBody, T]): AsReal[Future[RestResponse], Future[T]] =
    AsReal.create(_.mapNow {
      case RestResponse(200, body) => bodyAsReal.asReal(body)
      case RestResponse(code, body) => throw HttpErrorException(code, body.contentOpt.toOptArg)
    })
}
