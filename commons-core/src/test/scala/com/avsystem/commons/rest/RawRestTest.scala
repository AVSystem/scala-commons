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
}
object UserApi extends RestApiCompanion[UserApi]

trait RestTestApi {
  @Prefix("") def self: UserApi
  def subApi(id: Int, @Query query: String): UserApi
}
object RestTestApi extends RestApiCompanion[RestTestApi]

class RawRestTest extends FunSuite with ScalaFutures {
  def repr(req: RestRequest): String = {
    val pathRepr = req.headers.path.map(_.value).mkString("/")
    val queryRepr = req.headers.query.iterator
      .map({ case (k, v) => s"$k=${v.value}" }).mkStringOrEmpty("?", "&", "")
    val hasHeaders = req.headers.headers.nonEmpty
    val headersRepr = req.headers.headers.iterator
      .map({ case (n, v) => s"$n: ${v.value}" }).mkStringOrEmpty("\n", "\n", "\n")

    val contentRepr =
      if (req.body.content.isEmpty) ""
      else s"${if (hasHeaders) "" else " "}${req.body.mimeType}\n${req.body.content}"
    s"-> ${req.method} $pathRepr$queryRepr$headersRepr$contentRepr"
  }

  def repr(resp: RestResponse): String = {
    val contentRepr =
      if (resp.body.content.isEmpty) ""
      else s" ${resp.body.mimeType}\n${resp.body.content}"
    s"<- ${resp.code}$contentRepr"
  }

  class RestTestApiImpl(id: Int, query: String) extends RestTestApi with UserApi {
    def self: UserApi = this
    def subApi(newId: Int, newQuery: String): UserApi = new RestTestApiImpl(newId, query + newQuery)
    def user(userId: String): Future[User] = Future.successful(User(userId, s"$userId-$id-$query"))
    def user(paf: String, awesome: Boolean, f: Int, user: User): Future[Unit] = Future.unit
  }

  var trafficLog: String = _

  val real: RestTestApi = new RestTestApiImpl(0, "")
  val serverHandle: RestRequest => Future[RestResponse] = request => {
    RawRest.asHandleRequest(real).apply(request).andThenNow {
      case Success(response) =>
        trafficLog = s"${repr(request)}\n${repr(response)}\n"
    }
  }

  val realProxy: RestTestApi = RawRest.fromHandleRequest[RestTestApi](serverHandle)

  def testRestCall[T](call: RestTestApi => Future[T], expectedTraffic: String)(implicit pos: Position): Unit = {
    assert(call(realProxy).futureValue == call(real).futureValue)
    assert(trafficLog == expectedTraffic)
  }

  test("simple GET") {
    testRestCall(_.self.user("ID"),
      """-> GET user?userId=ID
        |<- 200 application/json
        |{"id":"ID","name":"ID-0-"}
        |""".stripMargin
    )
  }

  test("simple POST with path, header and query") {
    testRestCall(_.self.user("paf", awesome = true, 42, User("ID", "Fred")),
      """-> POST user/save/paf/moar/path?f=42
        |X-Awesome: true
        |application/json
        |{"id":"ID","name":"Fred"}
        |<- 200
        |""".stripMargin)
  }

  test("simple GET after prefix call") {
    testRestCall(_.subApi(1, "query").user("ID"),
      """-> GET subApi/1/user?query=query&userId=ID
        |<- 200 application/json
        |{"id":"ID","name":"ID-1-query"}
        |""".stripMargin
    )
  }
}
