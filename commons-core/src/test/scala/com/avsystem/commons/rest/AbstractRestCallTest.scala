package com.avsystem.commons
package rest

import com.avsystem.commons.rest.RawRest.HandleRequest
import com.avsystem.commons.serialization.HasGenCodec
import org.scalactic.source.Position
import org.scalatest.FunSuite
import org.scalatest.concurrent.ScalaFutures

case class RestEntity(id: String, name: String)
object RestEntity extends HasGenCodec[RestEntity]

trait RestTestApi {
  @GET def trivialGet: Future[Unit]

  @GET def failingGet: Future[Unit]

  @GET("a/b") def complexGet(
    @Path("p1") p1: Int, @Path p2: String,
    @Header("X-H1") h1: Int, @Header("X-H2") h2: String,
    q1: Int, @Query("q=2") q2: String
  ): Future[RestEntity]

  @POST def multiParamPost(
    @Path("p1") p1: Int, @Path p2: String,
    @Header("X-H1") h1: Int, @Header("X-H2") h2: String,
    @Query q1: Int, @Query("q=2") q2: String,
    b1: Int, @JsonBodyParam("b\"2") b2: String
  ): Future[RestEntity]

  @PUT("") def singleBodyPut(
    @Body entity: RestEntity
  ): Future[String]

  def prefix(
    p0: String,
    @Header("X-H0") h0: String,
    @Query q0: String
  ): RestTestSubApi
}
object RestTestApi extends RestApiCompanion[RestTestApi] {
  val Impl: RestTestApi = new RestTestApi {
    def trivialGet: Future[Unit] = Future.unit
    def failingGet: Future[Unit] = Future.failed(HttpErrorException(503, "nie"))
    def complexGet(p1: Int, p2: String, h1: Int, h2: String, q1: Int, q2: String): Future[RestEntity] =
      Future.successful(RestEntity(s"$p1-$h1-$q1", s"$p2-$h2-$q2"))
    def multiParamPost(p1: Int, p2: String, h1: Int, h2: String, q1: Int, q2: String, b1: Int, b2: String): Future[RestEntity] =
      Future.successful(RestEntity(s"$p1-$h1-$q1-$b1", s"$p2-$h2-$q2-$b2"))
    def singleBodyPut(entity: RestEntity): Future[String] =
      Future.successful(entity.toString)
    def prefix(p0: String, h0: String, q0: String): RestTestSubApi =
      RestTestSubApi.impl(s"$p0-$h0-$q0")
  }
}

trait RestTestSubApi {
  @GET def subget(@Path p1: Int, @Header("X-H1") h1: Int, q1: Int): Future[String]
}
object RestTestSubApi extends RestApiCompanion[RestTestSubApi] {
  def impl(arg: String): RestTestSubApi = new RestTestSubApi {
    def subget(p1: Int, h1: Int, q1: Int): Future[String] = Future.successful(s"$arg-$p1-$h1-$q1")
  }
}

abstract class AbstractRestCallTest extends FunSuite with ScalaFutures {
  final val serverHandle: RawRest.HandleRequest =
    RawRest.asHandleRequest[RestTestApi](RestTestApi.Impl)

  def clientHandle: RawRest.HandleRequest

  lazy val proxy: RestTestApi =
    RawRest.fromHandleRequest[RestTestApi](clientHandle)

  def testCall[T](call: RestTestApi => Future[T])(implicit pos: Position): Unit =
    assert(call(proxy).wrapToTry.futureValue == call(RestTestApi.Impl).wrapToTry.futureValue)

  test("trivial GET") {
    testCall(_.trivialGet)
  }

  test("failing GET") {
    testCall(_.failingGet)
  }

  test("complex GET") {
    testCall(_.complexGet(0, "a/+&", 1, "b/+&", 2, "c/+&"))
  }

  test("multi-param body POST") {
    testCall(_.multiParamPost(0, "a/+&", 1, "b/+&", 2, "c/+&", 3, "l\"l"))
  }

  test("single body PUT") {
    testCall(_.singleBodyPut(RestEntity("id", "name")))
  }

  test("prefixed GET") {
    testCall(_.prefix("p0", "h0", "q0").subget(0, 1, 2))
  }
}

class DirectRestCallTest extends AbstractRestCallTest {
  def clientHandle: HandleRequest = serverHandle
}
