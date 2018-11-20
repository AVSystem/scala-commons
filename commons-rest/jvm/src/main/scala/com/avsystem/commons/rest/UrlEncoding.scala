package com.avsystem.commons
package rest

import java.net.{URLDecoder, URLEncoder}

object UrlEncoding {
  def encode(query: String, spaceAsPlus: Boolean): String = {
    val res = URLEncoder.encode(query, "UTF-8")
    if (spaceAsPlus) res else res.replace("+", "%20")
  }

  def decode(query: String, plusAsSpace: Boolean): String = {
    val pre = if (plusAsSpace) query else query.replace("+", "%2B")
    URLDecoder.decode(pre, "UTF-8")
  }
}
