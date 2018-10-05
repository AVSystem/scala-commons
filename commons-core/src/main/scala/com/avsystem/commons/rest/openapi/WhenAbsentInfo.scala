package com.avsystem.commons
package rest.openapi

import com.avsystem.commons.meta.{TypedMetadata, infer, reifyAnnot}
import com.avsystem.commons.rest.JsonValue
import com.avsystem.commons.rpc.AsRaw
import com.avsystem.commons.serialization.whenAbsent

case class WhenAbsentInfo[T](
  @reifyAnnot annot: whenAbsent[T],
  @infer("for @whenAbsent value: ") asJson: AsRaw[JsonValue, T]
) extends TypedMetadata[T] {
  val fallbackValue: Opt[JsonValue] =
    Try(annot.value).fold(_ => Opt.Empty, v => asJson.asRaw(v).opt)
}
