package com.avsystem.commons
package mongo

object KeyEscaper {
  private val escapeReplacements = List(
    "_" -> "__",
    "." -> "_d",
    "$" -> "_D"
  )

  private val unescapeReplacements = escapeReplacements.reverseMap(_.swap)

  private def replaceUsing(original: String, replacements: List[(String, String)]): String = {
    replacements.foldLeft(original) {
      case (s, (literal, replacement)) => s.replaceAllLiterally(literal, replacement)
    }
  }

  def escape(key: String): String = replaceUsing(key, escapeReplacements)
  def unescape(key: String): String = replaceUsing(key, unescapeReplacements)
}
