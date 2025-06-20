package com.avsystem.commons
package serialization

import org.scalatest.funsuite.AnyFunSuite

final case class RecordWithDefaults(
  @transientDefault a: String = "",
  b: Int = 42
) {
  @generated def c: String = s"$a-$b"
}
object RecordWithDefaults extends HasApplyUnapplyCodec[RecordWithDefaults]

final class CustomRecordWithDefaults(val a: String, val b: Int)
object CustomRecordWithDefaults extends HasApplyUnapplyCodec[CustomRecordWithDefaults] {
  def apply(@transientDefault a: String = "", b: Int = 42): CustomRecordWithDefaults =
    new CustomRecordWithDefaults(a, b)
  def unapply(crwd: CustomRecordWithDefaults): Opt[(String, Int)] =
    Opt((crwd.a, crwd.b))
}

final class CustomWrapper(val a: String)
object CustomWrapper extends HasApplyUnapplyCodec[CustomWrapper] {
  def apply(@transientDefault a: String = ""): CustomWrapper = new CustomWrapper(a)
  def unapply(cw: CustomWrapper): Opt[String] = Opt(cw.a)
}

final case class RecordWithOpts(
  @optionalParam abc: Opt[String] = Opt.Empty,
  @transientDefault flag: Opt[Boolean] = Opt.Empty,
  b: Int = 42,
)
object RecordWithOpts extends HasApplyUnapplyCodec[RecordWithOpts]

final case class SingleFieldRecordWithOpts(@optionalParam abc: Opt[String] = Opt.Empty)
object SingleFieldRecordWithOpts extends HasApplyUnapplyCodec[SingleFieldRecordWithOpts]

final case class SingleFieldRecordWithTD(@transientDefault abc: String = "abc")
object SingleFieldRecordWithTD extends HasApplyUnapplyCodec[SingleFieldRecordWithTD]

class ObjectSizeTest extends AnyFunSuite {
  test("computing object size") {
    assert(RecordWithDefaults.codec.size(RecordWithDefaults()) == 2)
    assert(RecordWithDefaults.codec.size(RecordWithDefaults("fuu")) == 3)
    assert(RecordWithOpts.codec.size(RecordWithOpts("abc".opt)) == 2)
    assert(RecordWithOpts.codec.size(RecordWithOpts("abc".opt, true.opt)) == 3)
    assert(RecordWithOpts.codec.size(RecordWithOpts()) == 1)
    assert(SingleFieldRecordWithOpts.codec.size(SingleFieldRecordWithOpts()) == 0)
    assert(SingleFieldRecordWithOpts.codec.size(SingleFieldRecordWithOpts("abc".opt)) == 1)
    assert(SingleFieldRecordWithTD.codec.size(SingleFieldRecordWithTD()) == 0)
    assert(SingleFieldRecordWithTD.codec.size(SingleFieldRecordWithTD("haha")) == 1)
    assert(CustomRecordWithDefaults.codec.size(CustomRecordWithDefaults()) == 1)
    assert(CustomRecordWithDefaults.codec.size(CustomRecordWithDefaults("fuu")) == 2)
    assert(CustomWrapper.codec.size(CustomWrapper()) == 0)
    assert(CustomWrapper.codec.size(CustomWrapper("fuu")) == 1)
  }

  test("computing object size with custom output") {
    val defaultIgnoringOutput = new SequentialOutput {
      override def customEvent[T](marker: CustomEventMarker[T], event: T): Boolean =
        marker match {
          case IgnoreTransientDefaultMarker => true
          case _ =>  super.customEvent(marker, event)
        }
      override def finish(): Unit = ()
    }
    assert(RecordWithDefaults.codec.size(RecordWithDefaults(), defaultIgnoringOutput.opt) == 3)
    assert(RecordWithDefaults.codec.size(RecordWithDefaults("fuu"), defaultIgnoringOutput.opt) == 3)
    assert(RecordWithOpts.codec.size(RecordWithOpts("abc".opt), defaultIgnoringOutput.opt) == 3)
    assert(RecordWithOpts.codec.size(RecordWithOpts("abc".opt, true.opt), defaultIgnoringOutput.opt) == 3)
    assert(RecordWithOpts.codec.size(RecordWithOpts(), defaultIgnoringOutput.opt) == 2)
    assert(SingleFieldRecordWithOpts.codec.size(SingleFieldRecordWithOpts(), defaultIgnoringOutput.opt) == 0) // @optionalParam field should NOT be counted
    assert(SingleFieldRecordWithOpts.codec.size(SingleFieldRecordWithOpts("abc".opt), defaultIgnoringOutput.opt) == 1)
    assert(SingleFieldRecordWithTD.codec.size(SingleFieldRecordWithTD(), defaultIgnoringOutput.opt) == 1) // @transientDefault field should be counted
    assert(SingleFieldRecordWithTD.codec.size(SingleFieldRecordWithTD("haha"), defaultIgnoringOutput.opt) == 1)
    assert(CustomRecordWithDefaults.codec.size(CustomRecordWithDefaults(), defaultIgnoringOutput.opt) == 2)
    assert(CustomRecordWithDefaults.codec.size(CustomRecordWithDefaults("fuu"), defaultIgnoringOutput.opt) == 2)
  }
}
