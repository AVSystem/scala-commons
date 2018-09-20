package com.avsystem.commons
package okhttp.rest

import okhttp3.OkHttpClient
import org.scalatest.{BeforeAndAfterAll, Suite}

trait UsesOkHttpClient extends BeforeAndAfterAll { this: Suite =>
  val client: OkHttpClient = new OkHttpClient()

  override protected def beforeAll(): Unit = {
    super.beforeAll()
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
  }
}
