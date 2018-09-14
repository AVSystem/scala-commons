package com.avsystem.commons
package jetty.rest.examples

import com.avsystem.commons.rest._

case class User(id: String, name: String, birthYear: Int)
object User extends RestDataCompanion[User]

trait UserApi {
  /** Returns newly created user */
  def createUser(name: String, birthYear: Int): Future[User]
}
object UserApi extends DefaultRestApiCompanion[UserApi]