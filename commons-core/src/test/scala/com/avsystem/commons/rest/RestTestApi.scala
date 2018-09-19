package com.avsystem.commons
package rest

import com.avsystem.commons.rest.openapi.description
import com.avsystem.commons.serialization.flatten

sealed trait BaseEntity
object BaseEntity extends RestDataCompanion[BaseEntity]

@description("Flat sealed entity with some serious cases")
@flatten sealed trait FlatBaseEntity extends BaseEntity
object FlatBaseEntity extends RestDataCompanion[FlatBaseEntity]

@description("REST entity")
case class RestEntity(
  @description("entity id") id: String,
  name: String,
  @description("recursive optional subentity") subentity: OptArg[RestEntity] = OptArg.Empty
) extends FlatBaseEntity
object RestEntity extends RestDataCompanion[RestEntity]

case class RestOtherEntity(fuu: Boolean, kek: List[String]) extends FlatBaseEntity

case object SingletonEntity extends FlatBaseEntity

trait RestTestApi {
  @GET def trivialGet: Future[Unit]
  @GET def failingGet: Future[Unit]
  @GET def moreFailingGet: Future[Unit]

  @description("A really complex GET operation")
  @GET("a/b") def complexGet(
    @Path("p1") p1: Int, @description("Very serious path parameter") @Path p2: String,
    @Header("X-H1") h1: Int, @Header("X-H2") h2: String,
    q1: Int, @Query("q=2") q2: String
  ): Future[RestEntity]

  @POST def multiParamPost(
    @Path("p1") p1: Int, @Path p2: String,
    @Header("X-H1") h1: Int, @Header("X-H2") h2: String,
    @Query q1: Int, @Query("q=2") q2: String,
    b1: Int, @BodyField("b\"2") @description("weird body field") b2: String
  ): Future[RestEntity]

  @PUT("") def singleBodyPut(
    @Body @description("REST entity description") entity: RestEntity
  ): Future[String]

  @FormBody
  @POST def formPost(@Query q1: String, p1: String, p2: Int): Future[String]

  def prefix(
    p0: String,
    @Header("X-H0") h0: String,
    @Query q0: String
  ): RestTestSubApi

  def complexParams(baseEntity: BaseEntity, flatBaseEntity: Opt[FlatBaseEntity] = Opt.Empty): Future[Unit]
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
    def formPost(q1: String, p1: String, p2: Int): Future[String] =
      Future.successful(s"$q1-$p1-$p2")
    def prefix(p0: String, h0: String, q0: String): RestTestSubApi =
      RestTestSubApi.impl(s"$p0-$h0-$q0")
    def complexParams(baseEntity: BaseEntity, flatBaseEntity: Opt[FlatBaseEntity]): Future[Unit] = Future.unit
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