package com.avsystem.commons
package jetty.rest.examples

import com.avsystem.commons.rest._

trait UserApi {
  /** Returns ID of newly created user */
  def createUser(name: String, birthYear: Int): Future[String]
}
object UserApi extends DefaultRestApiCompanion[UserApi]