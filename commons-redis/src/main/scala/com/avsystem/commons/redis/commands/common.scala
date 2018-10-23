package com.avsystem.commons
package redis.commands

import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion}

import scala.collection.mutable

sealed abstract class SortOrder(val name: String) extends NamedEnum
object SortOrder extends NamedEnumCompanion[SortOrder] {
  case object Asc extends SortOrder("ASC")
  case object Desc extends SortOrder("DESC")

  def apply(asc: Boolean): SortOrder =
    if (asc) Asc else Desc

  val values: List[SortOrder] = caseObjects
}

case class Cursor(raw: Long) extends AnyVal {
  override def toString: String = raw.toString
}
object Cursor {
  final val NoCursor = Cursor(0)
}

abstract class ParsedInfo(info: String, attrSeparator: String, nameValueSeparator: String) {
  protected val attrMap: BMap[String, String] = mutable.OpenHashMap() ++
    info.split(attrSeparator).iterator.map { attr =>
      val Array(name, value) = attr.split(nameValueSeparator, 2)
      (name, value)
    }

  override def toString: String = info
}
