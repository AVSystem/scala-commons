package com.avsystem.commons
package rest

import com.avsystem.commons.serialization.HasGenCodec
import org.scalactic.source.Position
import org.scalatest.FunSuite
import org.scalatest.concurrent.ScalaFutures

case class User(id: String, name: String)
object User extends HasGenCodec[User]

trait UserApi {
  @GET def user(userId: String): Future[User]

  @POST("user/save") def user(
    @Path("moar/path") paf: String,
    @Header("X-Awesome") awesome: Boolean,
    @Query("f") foo: Int,
    @Body user: User
  ): Future[Unit]

  def autopost(bodyarg: String): Future[String]
  def singleBodyAutopost(@Body body: String): Future[String]
}
object UserApi extends DefaultRestApiCompanion[UserApi]

trait RootApi {
  @Prefix("") def self: UserApi
  def subApi(id: Int, @Query query: String): UserApi
}
object RootApi extends DefaultRestApiCompanion[RootApi]

class RawRestTest extends FunSuite with ScalaFutures {
  def repr(body: HttpBody, inNewLine: Boolean = true): String = body match {
    case HttpBody.Empty => ""
    case HttpBody(content, mimeType) => s"${if (inNewLine) "" else " "}$mimeType\n$content"
  }

  def repr(req: RestRequest): String = {
    val pathRepr = req.headers.path.map(_.value).mkString("/", "/", "")
    val queryRepr = req.headers.query.iterator
      .map({ case (k, v) => s"$k=${v.value}" }).mkStringOrEmpty("?", "&", "")
    val hasHeaders = req.headers.headers.nonEmpty
    val headersRepr = req.headers.headers.iterator
      .map({ case (n, v) => s"$n: ${v.value}" }).mkStringOrEmpty("\n", "\n", "\n")
    s"-> ${req.method} $pathRepr$queryRepr$headersRepr${repr(req.body, hasHeaders)}".trim
  }

  def repr(resp: RestResponse): String =
    s"<- ${resp.code} ${repr(resp.body)}".trim

  class RootApiImpl(id: Int, query: String) extends RootApi with UserApi {
    def self: UserApi = this
    def subApi(newId: Int, newQuery: String): UserApi = new RootApiImpl(newId, query + newQuery)
    def user(userId: String): Future[User] = Future.successful(User(userId, s"$userId-$id-$query"))
    def user(paf: String, awesome: Boolean, f: Int, user: User): Future[Unit] = Future.unit
    def autopost(bodyarg: String): Future[String] = Future.successful(bodyarg.toUpperCase)
    def singleBodyAutopost(@Body body: String): Future[String] = Future.successful(body.toUpperCase)
  }

  var trafficLog: String = _

  val real: RootApi = new RootApiImpl(0, "")
  val serverHandle: RawRest.HandleRequest = request => {
    RawRest.asHandleRequest(real).apply(request).andThenNow {
      case Success(response) =>
        trafficLog = s"${repr(request)}\n${repr(response)}\n"
    }
  }

  val realProxy: RootApi = RawRest.fromHandleRequest[RootApi](serverHandle)

  def testRestCall[T](call: RootApi => Future[T], expectedTraffic: String)(implicit pos: Position): Unit = {
    assert(call(realProxy).futureValue == call(real).futureValue)
    assert(trafficLog == expectedTraffic)
  }

  test("simple GET") {
    testRestCall(_.self.user("ID"),
      """-> GET /user?userId=ID
        |<- 200 application/json
        |{"id":"ID","name":"ID-0-"}
        |""".stripMargin
    )
  }

  test("simple POST with path, header and query") {
    testRestCall(_.self.user("paf", awesome = true, 42, User("ID", "Fred")),
      """-> POST /user/save/paf/moar/path?f=42
        |X-Awesome: true
        |application/json
        |{"id":"ID","name":"Fred"}
        |<- 200
        |""".stripMargin)
  }

  test("auto POST") {
    testRestCall(_.self.autopost("bod"),
      """-> POST /autopost application/json
        |{"bodyarg":"bod"}
        |<- 200 application/json
        |"BOD"
        |""".stripMargin)
  }

  test("single body auto POST") {
    testRestCall(_.self.singleBodyAutopost("bod"),
      """-> POST /singleBodyAutopost application/json
        |"bod"
        |<- 200 application/json
        |"BOD"
        |""".stripMargin)
  }

  test("simple GET after prefix call") {
    testRestCall(_.subApi(1, "query").user("ID"),
      """-> GET /subApi/1/user?query=query&userId=ID
        |<- 200 application/json
        |{"id":"ID","name":"ID-1-query"}
        |""".stripMargin
    )
  }
}
