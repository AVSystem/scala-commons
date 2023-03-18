package com.avsystem.commons
package serialization

import org.scalatest.funsuite.AnyFunSuite

case class RecordWithDefaults(
  @transientDefault a: String = "",
  b: Int = 42
) {
  @generated def c: String = s"$a-$b"
}
object RecordWithDefaults extends HasApplyUnapplyCodec[RecordWithDefaults]

class CustomRecordWithDefaults(val a: String, val b: Int)
object CustomRecordWithDefaults extends HasApplyUnapplyCodec[CustomRecordWithDefaults] {
  def apply(@transientDefault a: String = "", b: Int = 42): CustomRecordWithDefaults =
    new CustomRecordWithDefaults(a, b)
  def unapply(crwd: CustomRecordWithDefaults): Opt[(String, Int)] =
    Opt((crwd.a, crwd.b))
}

class CustomWrapper(val a: String)
object CustomWrapper extends HasApplyUnapplyCodec[CustomWrapper] {
  def apply(@transientDefault a: String = ""): CustomWrapper = new CustomWrapper(a)
  def unapply(cw: CustomWrapper): Opt[String] = Opt(cw.a)
}

class ObjectSizeTest extends AnyFunSuite {
  test("computing object size") {
    assert(RecordWithDefaults.codec.size(RecordWithDefaults()) == 2)
    assert(RecordWithDefaults.codec.size(RecordWithDefaults("fuu")) == 3)
    assert(CustomRecordWithDefaults.codec.size(CustomRecordWithDefaults()) == 1)
    assert(CustomRecordWithDefaults.codec.size(CustomRecordWithDefaults("fuu")) == 2)
    assert(CustomWrapper.codec.size(CustomWrapper()) == 0)
    assert(CustomWrapper.codec.size(CustomWrapper("fuu")) == 1)
  }
}
