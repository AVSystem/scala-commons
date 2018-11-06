package com.avsystem.commons
package rest

import scala.scalajs.js.URIUtils

object UrlEncoding {
  def encode(str: String): String = URIUtils.encodeURIComponent(str)
  def decode(str: String): String = URIUtils.decodeURIComponent(str)
}
