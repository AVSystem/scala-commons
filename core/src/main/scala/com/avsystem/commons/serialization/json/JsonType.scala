package com.avsystem.commons
package serialization.json

import com.avsystem.commons.misc.{AbstractValueEnum, AbstractValueEnumCompanion, EnumCtx}
import com.avsystem.commons.serialization.InputMetadata

final class JsonType(implicit enumCtx: EnumCtx) extends AbstractValueEnum
object JsonType extends AbstractValueEnumCompanion[JsonType] with InputMetadata[JsonType] {
  final val list, `object`, number, string, boolean, `null`: Value = new JsonType
}
