package com.avsystem.commons
package rest

import java.net.{URLDecoder, URLEncoder}

object UrlEncoding {
  def encode(str: String): String = URLEncoder.encode(str, "UTF-8")
  def decode(str: String): String = URLDecoder.decode(str, "UTF-8")
}
