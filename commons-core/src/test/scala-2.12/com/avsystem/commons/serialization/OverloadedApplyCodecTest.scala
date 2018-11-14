package com.avsystem.commons
package serialization

class OverloadedApplyCodecTest extends CodecTestBase {
  case class OverloadedApply(private val fields: Map[String, Int])
  object OverloadedApply extends HasGenCodec[OverloadedApply] {
    def apply(fields: Seq[Int]): OverloadedApply = OverloadedApply(fields.toMapBy(_.toString))
    def unapply(oa: OverloadedApply): Option[Seq[Int]] = Some(oa.fields.values.toSeq)
  }

  test("overloaded apply/unapply") {
    testWriteRead(OverloadedApply(List(1, 2, 3)),
      Map("fields" -> List(1, 2, 3))
    )
  }
}
