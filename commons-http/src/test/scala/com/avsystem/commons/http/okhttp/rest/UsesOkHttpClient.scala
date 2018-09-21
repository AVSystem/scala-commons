package com.avsystem.commons
package http.okhttp.rest

import okhttp3.OkHttpClient
import org.scalatest.{BeforeAndAfterAll, Suite}

trait UsesOkHttpClient {
  val client: OkHttpClient = new OkHttpClient()
}
