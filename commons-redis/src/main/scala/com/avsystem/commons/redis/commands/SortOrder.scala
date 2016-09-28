package com.avsystem.commons
package redis.commands

import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion}

sealed abstract class SortOrder(val name: String) extends NamedEnum
object SortOrder extends NamedEnumCompanion[SortOrder] {
  case object Asc extends SortOrder("ASC")
  case object Desc extends SortOrder("DESC")

  def apply(asc: Boolean): SortOrder =
    if (asc) Asc else Desc

  val values: List[SortOrder] = caseObjects
}
