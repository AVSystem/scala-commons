package com.avsystem.commons
package jetty.rest.examples

import com.avsystem.commons.rest._

trait UserApi {
  @GET def getUsername(userId: String): Future[String]
}
object UserApi extends RestApiCompanion[UserApi]