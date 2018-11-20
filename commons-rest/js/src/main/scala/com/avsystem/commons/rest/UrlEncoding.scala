package com.avsystem.commons
package rest

import scala.scalajs.js.URIUtils

object UrlEncoding {
  def encode(query: String, spaceAsPlus: Boolean): String = {
    val res = URIUtils.encodeURIComponent(query)
      .replace("!", "%21")
      .replace("'", "%27")
      .replace("(", "%28")
      .replace(")", "%29")
      .replace("~", "%7E")

    if (spaceAsPlus) res.replace("%20", "+") else res
  }

  def decode(query: String, plusAsSpace: Boolean): String = {
    val pre = if (plusAsSpace) query.replace("+", "%20") else query
    URIUtils.decodeURIComponent(pre)
  }
}
