package com.avsystem.commons
package rpc

import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion}
import com.avsystem.commons.serialization.GenCodec

sealed abstract class Tag[T](implicit val codec: GenCodec[T]) extends NamedEnum with Product {
  def name: String = productPrefix
}
object Tag extends NamedEnumCompanion[Tag[?]] {
  def apply[T](implicit tag: Tag[T]): Tag[T] = tag

  implicit case object String extends Tag[String]
  implicit case object Int extends Tag[Int]

  val values: ISeq[Tag[?]] = caseObjects

  implicit def tagCodec[T]: GenCodec[Tag[T]] =
    codec.asInstanceOf[GenCodec[Tag[T]]]
}
