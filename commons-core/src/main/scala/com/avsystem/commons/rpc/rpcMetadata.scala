package com.avsystem.commons
package rpc

import com.avsystem.commons.serialization.{HasGenCodec, transparent}

/**
  * This trait must be extended by all method metadata classes and all parameter metadata classes.
  * For method metadata, type parameter `T` will be matched against each real method result type.
  * For parameter metadata, type parameter `T` will be matched against each real parameter type.
  */
trait TypedMetadata[T]

/**
  * Information about real parameter flags and modifiers as defined in Scala code.
  */
@transparent
final case class ParamFlags(rawFlags: Int) extends AnyVal {

  import ParamFlags._

  def |(other: ParamFlags): ParamFlags = new ParamFlags(rawFlags | other.rawFlags)
  def &(other: ParamFlags): ParamFlags = new ParamFlags(rawFlags & other.rawFlags)
  def ^(other: ParamFlags): ParamFlags = new ParamFlags(rawFlags ^ other.rawFlags)
  def unary_~ : ParamFlags = new ParamFlags(~rawFlags)

  def hasFlags(flags: ParamFlags): Boolean = (this & flags) == flags

  def isImplicit: Boolean = hasFlags(Implicit)
  def isByName: Boolean = hasFlags(ByName)
  def isRepeated: Boolean = hasFlags(Repeated)
  def hasDefaultValue: Boolean = hasFlags(HasDefaultValue)
  def isSynthetic: Boolean = hasFlags(Synthetic)

  override def toString: String = {
    def repr(flags: ParamFlags, r: String): Opt[String] =
      r.opt.filter(_ => hasFlags(flags))

    List(
      repr(Implicit, "implicit"),
      repr(ByName, "byName"),
      repr(Repeated, "repeated"),
      repr(HasDefaultValue, "hasDefaultValue"),
      repr(Synthetic, "synthetic")
    ).flatten.mkString(",")
  }
}

object ParamFlags extends HasGenCodec[ParamFlags] {
  final val Empty: ParamFlags = new ParamFlags(0)
  final val Implicit: ParamFlags = new ParamFlags(1 << 0)
  final val ByName: ParamFlags = new ParamFlags(1 << 1)
  final val Repeated: ParamFlags = new ParamFlags(1 << 2)
  final val HasDefaultValue: ParamFlags = new ParamFlags(1 << 3)
  final val Synthetic: ParamFlags = new ParamFlags(1 << 4)
}

/**
  * Information about real parameter position in its method. All indices start from 0.
  *
  * @param index       overall index of the parameter, among all parameter lists
  * @param indexOfList index of parameter list that this parameter belongs to
  * @param indexInList index of the parameter inside its parameter list
  * @param indexInRaw  index of the parameter in its corresponding `@multi` metadata parameter
  *                    (or zero if not `@multi`)
  */
final case class ParamPosition(
  index: Int,
  indexOfList: Int,
  indexInList: Int,
  indexInRaw: Int
)
object ParamPosition extends HasGenCodec[ParamPosition]
