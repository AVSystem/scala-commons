package com.avsystem.commons
package jetty.rest.examples

import com.avsystem.commons.jetty.rest.RestClient
import org.eclipse.jetty.client.HttpClient

import scala.concurrent.Await
import scala.concurrent.duration._

object ClientMain {
  def main(args: Array[String]): Unit = {
    val client = new HttpClient
    client.start()

    val proxy = RestClient[UserApi](client, "http://localhost:9090/")

    // just for this example, normally it's not recommended
    import scala.concurrent.ExecutionContext.Implicits.global

    val result = proxy.createUser("Fred", 1990)
      .andThen({ case _ => client.stop() })
      .andThen {
        case Success(user) => println(s"User ${user.id} created")
        case Failure(cause) => cause.printStackTrace()
      }

    // just wait until future is complete so that main thread doesn't finish prematurely
    Await.result(result, 10.seconds)
  }
}
