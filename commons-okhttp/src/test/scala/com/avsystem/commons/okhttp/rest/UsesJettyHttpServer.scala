package com.avsystem.commons
package okhttp.rest

import org.eclipse.jetty.server.Server
import org.scalatest.{BeforeAndAfterAll, Suite}

trait UsesJettyHttpServer extends BeforeAndAfterAll { this: Suite =>

  val port = 9090
  val server = new Server(port)
  val scheme = "http"
  val host = "localhost"

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
