package com.avsystem.commons
package mongo

object KeyEscaper {
  private final val Esc = '\\'
  private final val EscSub = '\\'
  private final val EscRepl = s"$Esc$EscSub"

  private final val Dot = '.'
  private final val DotSub = '_'
  private final val DotRepl = s"$Esc$DotSub"

  private final val Dollar = '$'
  private final val DollarSub = '$'
  private final val DollarRepl = s"$Esc$DollarSub"

  private def escapeSpecials(str: String, start: Int, sb: JStringBuilder): Unit = {
    var i = start
    val len = str.length
    while (i < len) {
      str.charAt(i) match {
        case Dot => sb.append(DotRepl)
        case Esc => sb.append(EscRepl)
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
        if (c == Dot || c == Esc) specials += 1
        i += 1
      }

      if (key.charAt(0) == Dollar) {
        val sb = new JStringBuilder(key.length + 1 + specials)
        sb.append(DollarRepl)
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
        case Esc =>
          str.charAt(i + 1) match {
            case EscSub => sb.append(Esc)
            case DotSub => sb.append(Dot)
            case DollarSub => sb.append(Dollar)
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
      if (c == Esc) {
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
