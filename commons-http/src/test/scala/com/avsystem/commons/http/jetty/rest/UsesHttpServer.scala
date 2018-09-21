package com.avsystem.commons
package http.jetty.rest

import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.{ServletHandler, ServletHolder}
import org.scalatest.{BeforeAndAfterAll, Suite}

trait UsesHttpServer extends BeforeAndAfterAll { this: Suite =>
  val port: Int = 9090
  val server: Server = new Server(port)
  val scheme: String = "http"
  val host: String = "localhost"
  val basePath: String = "api"
  val baseUrl = s"$scheme://$host:$port/$basePath"

  protected def setupServer(server: Server): Unit

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    setupServer(server)
    server.start()
  }

  override protected def afterAll(): Unit = {
    server.stop()
    super.afterAll()
  }
}
