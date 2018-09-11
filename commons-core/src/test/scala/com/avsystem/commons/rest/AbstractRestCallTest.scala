package com.avsystem.commons
package rest

import com.avsystem.commons.rest.RawRest.HandleRequest
import com.avsystem.commons.rest.openapi.{Info, RestDataCompanion}
import com.avsystem.commons.serialization.flatten
import com.avsystem.commons.serialization.json.{JsonOptions, JsonStringOutput}
import org.scalactic.source.Position
import org.scalatest.FunSuite
import org.scalatest.concurrent.ScalaFutures

sealed trait BaseEntity
object BaseEntity extends RestDataCompanion[BaseEntity]

@flatten sealed trait FlatBaseEntity extends BaseEntity
object FlatBaseEntity extends RestDataCompanion[FlatBaseEntity]

case class RestEntity(id: String, name: String, subentity: OptArg[RestEntity] = OptArg.Empty) extends FlatBaseEntity
object RestEntity extends RestDataCompanion[RestEntity]

case object SingletonEntity extends FlatBaseEntity

trait RestTestApi {
  @GET def trivialGet: Future[Unit]
  @GET def failingGet: Future[Unit]
  @GET def moreFailingGet: Future[Unit]

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

  def complexParams(baseEntity: BaseEntity, flatBaseEntity: FlatBaseEntity): Future[Unit]
}
object RestTestApi extends DefaultRestApiCompanion[RestTestApi] {
  val Impl: RestTestApi = new RestTestApi {
    def trivialGet: Future[Unit] = Future.unit
    def failingGet: Future[Unit] = Future.failed(HttpErrorException(503, "nie"))
    def moreFailingGet: Future[Unit] = throw HttpErrorException(503, "nie")
    def complexGet(p1: Int, p2: String, h1: Int, h2: String, q1: Int, q2: String): Future[RestEntity] =
      Future.successful(RestEntity(s"$p1-$h1-$q1", s"$p2-$h2-$q2"))
    def multiParamPost(p1: Int, p2: String, h1: Int, h2: String, q1: Int, q2: String, b1: Int, b2: String): Future[RestEntity] =
      Future.successful(RestEntity(s"$p1-$h1-$q1-$b1", s"$p2-$h2-$q2-$b2"))
    def singleBodyPut(entity: RestEntity): Future[String] =
      Future.successful(entity.toString)
    def prefix(p0: String, h0: String, q0: String): RestTestSubApi =
      RestTestSubApi.impl(s"$p0-$h0-$q0")
    def complexParams(baseEntity: BaseEntity, flatBaseEntity: FlatBaseEntity): Future[Unit] = Future.unit
  }
}

trait RestTestSubApi {
  @GET def subget(@Path p1: Int, @Header("X-H1") h1: Int, q1: Int): Future[String]
}
object RestTestSubApi extends DefaultRestApiCompanion[RestTestSubApi] {
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
    assert(call(proxy).wrapToTry.futureValue == call(RestTestApi.Impl).catchFailures.wrapToTry.futureValue)

  test("trivial GET") {
    testCall(_.trivialGet)
  }

  test("failing GET") {
    testCall(_.failingGet)
  }

  test("more failing GET") {
    testCall(_.moreFailingGet)
  }

  test("complex GET") {
    testCall(_.complexGet(0, "a/+&", 1, "b/+&", 2, "ć/+&"))
  }

  test("multi-param body POST") {
    testCall(_.multiParamPost(0, "a/+&", 1, "b/+&", 2, "ć/+&", 3, "l\"l"))
  }

  test("single body PUT") {
    testCall(_.singleBodyPut(RestEntity("id", "señor")))
  }

  test("prefixed GET") {
    testCall(_.prefix("p0", "h0", "q0").subget(0, 1, 2))
  }
}

class DirectRestCallTest extends AbstractRestCallTest {
  def clientHandle: HandleRequest = serverHandle

  test("openapi") {
    val openapi = RestTestApi.openapiMetadata.openapi(Info("Test API", "0.1"))
    println(JsonStringOutput.write(openapi, JsonOptions.Pretty))
  }
}
