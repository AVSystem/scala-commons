package com.avsystem.commons
package rest

import com.avsystem.commons.serialization.HasGenCodec

case class User(id: String, name: String)
object User extends HasGenCodec[User]

trait RestTestApi {
  def subApi(id: Int, @Query query: String): RestTestApi

  @GET def user(userId: String): Future[User]
  @POST def user(@Body user: User): Future[Unit]
}
object RestTestApi extends RestApiCompanion[RestTestApi]
