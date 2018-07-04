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
  @POST("user/save") def user(@Body user: User): Future[Unit]
}
object UserApi extends RestApiCompanion[UserApi]

trait RestTestApi {
  @Prefix("") def self: UserApi
  def subApi(id: Int, @Query query: String): UserApi
}
object RestTestApi extends RestApiCompanion[RestTestApi]

class RawRestTest extends FunSuite with ScalaFutures {
  test("round trip test") {
    class RestTestApiImpl(id: Int, query: String) extends RestTestApi with UserApi {
      def self: UserApi = this
      def subApi(newId: Int, newQuery: String): UserApi = new RestTestApiImpl(newId, query + newQuery)
      def user(userId: String): Future[User] = Future.successful(User(userId, s"$userId-$id-$query"))
      def user(user: User): Future[Unit] = Future.unit
    }

    val callRecord = new MListBuffer[(RestRequest, RestResponse)]

    val real: RestTestApi = new RestTestApiImpl(0, "")
    val serverHandle: RestRequest => Future[RestResponse] = request => {
      RawRest.asHandleRequest(real).apply(request).andThenNow {
        case Success(response) => callRecord += ((request, response))
      }
    }

    val realProxy: RestTestApi = RawRest.fromHandleRequest[RestTestApi](serverHandle)

    def assertSame[T](call: RestTestApi => Future[T])(implicit pos: Position): Unit =
      assert(call(realProxy).futureValue == call(real).futureValue)

    assertSame(_.self.user("ID"))
    assertSame(_.subApi(1, "query").user("ID"))
  }
}
