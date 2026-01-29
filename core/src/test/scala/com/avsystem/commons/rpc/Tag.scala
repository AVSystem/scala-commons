package com.avsystem.commons
package rpc

import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion}
import com.avsystem.commons.serialization.GenCodec

sealed abstract class Tag[T](using val codec: GenCodec[T]) extends NamedEnum with Product {
  def name: String = productPrefix
}
object Tag extends NamedEnumCompanion[Tag[?]] {
  // todo: use given
  implicit case object String extends Tag[String]
  implicit case object Int extends Tag[Int]
  val values: ISeq[Tag[?]] = caseObjects
  def apply[T](using tag: Tag[T]): Tag[T] = tag
  given [T] => GenCodec[Tag[T]] = given_GenCodec_T.asInstanceOf[GenCodec[Tag[T]]]
}
