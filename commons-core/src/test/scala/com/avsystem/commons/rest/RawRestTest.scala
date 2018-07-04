package com.avsystem.commons
package rest

import com.avsystem.commons.serialization.HasGenCodec
import org.scalactic.source.Position
import org.scalatest.FunSuite
import org.scalatest.concurrent.ScalaFutures

case class User(id: String, name: String)
object User extends HasGenCodec[User]

trait RestTestApi {
  def subApi(id: Int, @Query query: String): RestTestApi

  @GET def user(userId: String): Future[User]
  @POST def user(@Body user: User): Future[Unit]
}
object RestTestApi extends RestApiCompanion[RestTestApi]

class RawRestTest extends FunSuite with ScalaFutures {
  test("round trip test") {
    class RestTestApiImpl(id: Int, query: String) extends RestTestApi {
      def subApi(newId: Int, newQuery: String): RestTestApi = new RestTestApiImpl(newId, query + newQuery)
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

    val realProxy: RestTestApi =
      RestTestApi.restAsRealRaw.asReal(RawRest(serverHandle))

    def assertSame[T](call: RestTestApi => Future[T])(implicit pos: Position): Unit =
      assert(call(realProxy).futureValue == call(real).futureValue)

    assertSame(_.user("ID"))
    assertSame(_.subApi(1, "query").user("ID"))
  }
}
