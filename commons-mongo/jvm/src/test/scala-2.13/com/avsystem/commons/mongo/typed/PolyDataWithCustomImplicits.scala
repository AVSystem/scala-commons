package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.meta.MacroInstances
import com.avsystem.commons.serialization.GenCodec

case class CustomWrappy(value: String)

object CustomImplicits {
  implicit val customWrappyCodec: GenCodec[CustomWrappy] =
    GenCodec.transformed[CustomWrappy, String](_.value, CustomWrappy)
}

abstract class CustomPolyDataCompanion[D[_]](
  implicit instances: MacroInstances[CustomImplicits.type, MongoPolyAdtInstances[D]],
) extends AbstractMongoPolyDataCompanion[CustomImplicits.type, D](CustomImplicits)

/**
  * This class tests (through its compilation) if implicit resolution conflicts that were
  * previously present in [[MongoPolyAdtInstances]] are fixed.
  */
case class PolyDataWithCustomImplicits[+T](wrappy: CustomWrappy, value: List[T])
object PolyDataWithCustomImplicits extends CustomPolyDataCompanion[PolyDataWithCustomImplicits]
