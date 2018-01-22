package com.avsystem.commons
package mongo.text

import com.avsystem.commons.misc.{AbstractValueEnum, AbstractValueEnumCompanion, EnumCtx}

/**
  * Language supported by MongoDB text search.
  *
  * @param code ISO 639-1, ISO 639-3 or RLP code used by Mongo $text operator
  * @see [[https://docs.mongodb.com/manual/reference/text-search-languages/#text-search-languages]]
  */
final class TextSearchLanguage(val code: String)(implicit enumCtx: EnumCtx) extends AbstractValueEnum
object TextSearchLanguage extends AbstractValueEnumCompanion[TextSearchLanguage] {

  /**
    * Uses simple tokenization with no list of stop words and no stemming.
    */
  final val None: Value = new TextSearchLanguage("none")

  final val Danish: Value = new TextSearchLanguage("da")
  final val Dutch: Value = new TextSearchLanguage("nl")
  final val English: Value = new TextSearchLanguage("en")
  final val Finnish: Value = new TextSearchLanguage("fi")
  final val French: Value = new TextSearchLanguage("fr")
  final val German: Value = new TextSearchLanguage("de")
  final val Hungarian: Value = new TextSearchLanguage("hu")
  final val Italian: Value = new TextSearchLanguage("it")
  final val Norwegian: Value = new TextSearchLanguage("nb")
  final val Portuguese: Value = new TextSearchLanguage("pt")
  final val Romanian: Value = new TextSearchLanguage("ro")
  final val Russian: Value = new TextSearchLanguage("ru")
  final val Spanish: Value = new TextSearchLanguage("es")
  final val Swedish: Value = new TextSearchLanguage("sv")
  final val Turkish: Value = new TextSearchLanguage("tr")
  final val Arabic: Value = new TextSearchLanguage("ara")
  final val Dari: Value = new TextSearchLanguage("prs")
  final val IranianPersian: Value = new TextSearchLanguage("pes")
  final val Urdu: Value = new TextSearchLanguage("urd")
  final val SimplifiedChinese: Value = new TextSearchLanguage("zhs")
  final val Hans: Value = SimplifiedChinese
  final val TraditionalChinese: Value = new TextSearchLanguage("zht")
  final val Hant: Value = TraditionalChinese
}
