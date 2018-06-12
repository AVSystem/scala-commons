package com.avsystem.commons
package serialization

sealed trait SealedBaseThatHasGenCodec
object SealedBaseThatHasGenCodec extends HasGenCodec[SealedBaseThatHasGenCodec] {
  case object CaseObject extends SealedBaseThatHasGenCodec
  case class CaseClass(str: String) extends SealedBaseThatHasGenCodec
  object CaseClass extends HasGenCodec[CaseClass]

  sealed trait InnerBase extends SealedBaseThatHasGenCodec
  object InnerBase {
    case object InnerCaseObject extends InnerBase
    case class InnerCaseClass(str: String = "kek") extends InnerBase
    object InnerCaseClass extends HasGenCodec[InnerCaseClass]
  }
}
