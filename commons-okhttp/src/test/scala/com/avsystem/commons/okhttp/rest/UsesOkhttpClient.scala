package com.avsystem.commons
package okhttp.rest

import okhttp3.OkHttpClient
import org.scalatest.{BeforeAndAfterAll, Suite}

trait UsesOkhttpClient extends BeforeAndAfterAll { this: Suite =>
  val client: OkHttpClient = new OkHttpClient()
}
