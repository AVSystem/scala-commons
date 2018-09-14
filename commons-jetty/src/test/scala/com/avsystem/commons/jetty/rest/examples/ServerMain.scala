package com.avsystem.commons
package jetty.rest.examples

import com.avsystem.commons.jetty.rest.RestHandler
import org.eclipse.jetty.server.Server

class UserApiImpl extends UserApi {
  def createUser(name: String, birthYear: Int): Future[User] =
    Future.successful(User(s"$name-ID", name, birthYear))
}

object ServerMain {
  def main(args: Array[String]): Unit = {
    val server = new Server(9090)
    server.setHandler(RestHandler[UserApi](new UserApiImpl))
    server.start()
    server.join()
  }
}
