package com.avsystem.commons
package serialization

import com.avsystem.commons.annotation.positioned
import com.avsystem.commons.meta._

sealed trait GenInfo[T] extends TypedMetadata[T] {
  def sourceName: String
  def annotName: Opt[name]
  def rawName: String = annotName.fold(sourceName)(_.name)
}

case class GenParamInfo[T](
  @reifyName sourceName: String,
  @optional @reifyAnnot annotName: Opt[name],
  @isAnnotated[whenAbsent[T]] hasWhenAbsent: Boolean,
  @isAnnotated[transientDefault] transientDefault: Boolean,
  @isAnnotated[outOfOrder] outOfOrder: Boolean,
  @reifyFlags flags: ParamFlags
) extends GenInfo[T]

sealed trait GenCodecStructure[T] extends GenInfo[T] {
  def flags: TypeFlags
}

@positioned(positioned.here) case class GenUnionInfo[T](
  @reifyFlags flags: TypeFlags,
  @reifyName sourceName: String,
  @optional @reifyAnnot annotName: Opt[name],
  @optional @reifyAnnot flatten: Opt[flatten]
) extends GenCodecStructure[T]
object GenUnionInfo extends AdtMetadataCompanion[GenUnionInfo]

@positioned(positioned.here) case class GenCaseInfo[T](
  @reifyFlags flags: TypeFlags,
  @reifyName sourceName: String,
  @optional @reifyAnnot annotName: Opt[name],
  @isAnnotated[transparent] transparent: Boolean,
  @isAnnotated[defaultCase] defaultCase: Boolean
) extends GenCodecStructure[T]
object GenCaseInfo extends AdtMetadataCompanion[GenCaseInfo]
