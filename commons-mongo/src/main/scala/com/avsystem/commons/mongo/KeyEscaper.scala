package com.avsystem.commons
package mongo

import java.lang.{StringBuilder => JStringBuilder}

object KeyEscaper {
  private def escapeSpecials(str: String, start: Int, sb: JStringBuilder): Unit = {
    var i = start
    val len = str.length
    while (i < len) {
      str.charAt(i) match {
        case '.' => sb.append("\\_")
        case '\\' => sb.append("\\\\")
        case c => sb.append(c)
      }
      i += 1
    }
  }

  def escape(key: String): String = {
    if (key.isEmpty) {
      key
    } else {
      var i = 0
      var specials = 0
      while (i < key.length) {
        val c = key.charAt(i)
        if (c == '.' || c == '\\') specials += 1
        i += 1
      }

      if (key.charAt(0) == '$') {
        val sb = new JStringBuilder(key.length + 1 + specials)
        sb.append("\\$")
        if (specials > 0) {
          escapeSpecials(key, 1, sb)
        } else {
          sb.append(key, 1, key.length)
        }
        sb.toString
      } else {
        if (specials > 0) {
          val sb = new JStringBuilder(key.length + specials)
          escapeSpecials(key, 0, sb)
          sb.toString
        } else {
          key
        }
      }
    }
  }

  private def unescapeSpecials(str: String, sb: JStringBuilder): Unit = {
    var i = 0
    while (i < str.length) {
      str.charAt(i) match {
        case '\\' =>
          str.charAt(i + 1) match {
            case '\\' => sb.append('\\')
            case '_' => sb.append('.')
            case '$' => sb.append('$')
          }
          i += 2
        case c =>
          sb.append(c)
          i += 1
      }
    }
  }

  def unescape(key: String): String = {
    var specials = 0
    var i = 0

    while (i < key.length) {
      val c = key.charAt(i)
      if (c == '\\') {
        specials += 1
        i += 2
      } else {
        i += 1
      }
    }

    if (specials > 0) {
      val sb = new JStringBuilder(key.length - specials)
      unescapeSpecials(key, sb)
      sb.toString
    } else {
      key
    }
  }
}
