package com.avsystem.commons
package jetty.rpc

import org.eclipse.jetty.client.HttpClient
import org.scalatest.{BeforeAndAfterAll, Suite}

trait UsesHttpClient extends BeforeAndAfterAll { this: Suite =>
  val client: HttpClient = new HttpClient()

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    client.start()
  }

  override protected def afterAll(): Unit = {
    client.stop()
    super.afterAll()
  }
}
