package com.avsystem.commons
package serialization

import com.avsystem.commons.meta._

sealed trait GenInfo[T] extends TypedMetadata[T] {
  def sourceName: String
  def annotName: Opt[name]
  def rawName: String = annotName.fold(sourceName)(_.name)
}

case class GenParamInfo[T](
  @reifyName sourceName: String,
  @optional @reifyAnnot annotName: Opt[name],
  @optional @reifyAnnot whenAbsent: Opt[whenAbsent[T]],
  @isAnnotated[transientDefault] transientDefault: Boolean,
  @isAnnotated[outOfOrder] outOfOrder: Boolean,
  @reifyFlags flags: ParamFlags
) extends GenInfo[T] {
  val hasFallbackValue: Boolean =
    whenAbsent.fold(flags.hasDefaultValue)(wa => Try(wa.value).isSuccess)
}

sealed trait GenCodecStructure[T] extends GenInfo[T] {
  def flags: TypeFlags
}

case class GenUnionInfo[T](
  @reifyFlags flags: TypeFlags,
  @reifyName sourceName: String,
  @optional @reifyAnnot annotName: Opt[name],
  @optional @reifyAnnot flatten: Opt[flatten],
) extends GenCodecStructure[T]
object GenUnionInfo extends AdtMetadataCompanion[GenUnionInfo]

case class GenCaseInfo[T](
  @reifyFlags flags: TypeFlags,
  @reifyName sourceName: String,
  @optional @reifyAnnot annotName: Opt[name],
  @isAnnotated[transparent] transparent: Boolean,
  @isAnnotated[defaultCase] defaultCase: Boolean
) extends GenCodecStructure[T]
object GenCaseInfo extends AdtMetadataCompanion[GenCaseInfo]
