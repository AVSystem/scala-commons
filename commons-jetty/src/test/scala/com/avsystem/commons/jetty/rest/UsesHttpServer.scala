package com.avsystem.commons
package jetty.rest

import org.eclipse.jetty.server.Server
import org.scalatest.{BeforeAndAfterAll, Suite}

trait UsesHttpServer extends BeforeAndAfterAll { this: Suite =>
  val port: Int = 9090
  val server: Server = new Server(port)
  val baseUrl = s"http://localhost:$port"

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
