package com.avsystem.commons
package meta

import com.avsystem.commons.serialization.{HasGenCodec, transparent}

/**
  * This trait must be extended by all method metadata classes and all parameter metadata classes.
  * For method metadata, type parameter `T` will be matched against each real method result type.
  * For parameter metadata, type parameter `T` will be matched against each real parameter type.
  */
trait TypedMetadata[T]

/**
  * Captures case class parameter's default value. Used as type of ADT metadata parameter
  * annotated with [[reifyDefaultValue]].
  */
final class DefaultValue[T](dv: => T) {
  def value: T = dv
}

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
  private[this] var currentFlag: Int = 1
  private[this] def nextFlag(): ParamFlags = {
    val flag = currentFlag
    currentFlag = currentFlag << 1
    new ParamFlags(flag)
  }

  final val Empty: ParamFlags = new ParamFlags(0)
  final val Implicit: ParamFlags = nextFlag()
  final val ByName: ParamFlags = nextFlag()
  final val Repeated: ParamFlags = nextFlag()
  final val HasDefaultValue: ParamFlags = nextFlag()
  final val Synthetic: ParamFlags = nextFlag()
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

/**
  * Information about class or trait flags and modifiers as defined in Scala code.
  */
@transparent
final case class TypeFlags(rawFlags: Int) extends AnyVal {

  import TypeFlags._

  def |(other: TypeFlags): TypeFlags = new TypeFlags(rawFlags | other.rawFlags)
  def &(other: TypeFlags): TypeFlags = new TypeFlags(rawFlags & other.rawFlags)
  def ^(other: TypeFlags): TypeFlags = new TypeFlags(rawFlags ^ other.rawFlags)
  def unary_~ : TypeFlags = new TypeFlags(~rawFlags)

  def hasFlags(flags: TypeFlags): Boolean = (this & flags) == flags

  def isAbstract: Boolean = hasFlags(Abstract)
  def isFinal: Boolean = hasFlags(Final)
  def isSealed: Boolean = hasFlags(Sealed)
  def isCase: Boolean = hasFlags(Case)
  def isTrait: Boolean = hasFlags(Trait)
  def isObject: Boolean = hasFlags(Object)

  override def toString: String = {
    def repr(flags: TypeFlags, r: String): Opt[String] =
      r.opt.filter(_ => hasFlags(flags))

    List(
      repr(Abstract, "abstract"),
      repr(Final, "final"),
      repr(Sealed, "sealed"),
      repr(Case, "case"),
      repr(Trait, "trait"),
      repr(Object, "object")
    ).flatten.mkString(",")
  }
}

object TypeFlags extends HasGenCodec[TypeFlags] {
  private[this] var currentFlag: Int = 1
  private[this] def nextFlag(): TypeFlags = {
    val flag = currentFlag
    currentFlag = currentFlag << 1
    new TypeFlags(flag)
  }

  final val Empty: TypeFlags = new TypeFlags(0)
  final val Abstract: TypeFlags = nextFlag()
  final val Final: TypeFlags = nextFlag()
  final val Sealed: TypeFlags = nextFlag()
  final val Case: TypeFlags = nextFlag()
  final val Trait: TypeFlags = nextFlag()
  final val Object: TypeFlags = nextFlag()
}

/**
  * Information about method (also `val` or `var`) flags and modifiers as defined in Scala code.
  */
@transparent
final case class MethodFlags(rawFlags: Int) extends AnyVal {

  import MethodFlags._

  def |(other: MethodFlags): MethodFlags = new MethodFlags(rawFlags | other.rawFlags)
  def &(other: MethodFlags): MethodFlags = new MethodFlags(rawFlags & other.rawFlags)
  def ^(other: MethodFlags): MethodFlags = new MethodFlags(rawFlags ^ other.rawFlags)
  def unary_~ : MethodFlags = new MethodFlags(~rawFlags)

  def hasFlags(flags: MethodFlags): Boolean = (this & flags) == flags

  def isAbstract: Boolean = hasFlags(Abstract)
  def isFinal: Boolean = hasFlags(Final)
  def isLazy: Boolean = hasFlags(Lazy)
  def isGetter: Boolean = hasFlags(Getter)
  def isSetter: Boolean = hasFlags(Setter)
  def isVal: Boolean = isGetter && !isVar
  def isVar: Boolean = hasFlags(Var)

  def baseDecl: String = {
    val finalRepr = if(isFinal) "final " else ""
    val lazyRepr = if(isLazy) "lazy " else ""
    val kw = if(isVal) "val" else if(isVar && !isSetter) "var" else "def"
    s"$finalRepr$lazyRepr$kw"
  }

  override def toString: String = {
    def repr(flags: MethodFlags, r: String): Opt[String] =
      r.opt.filter(_ => hasFlags(flags))

    List(
      repr(Abstract, "abstract"),
      repr(Final, "final"),
      repr(Lazy, "lazy"),
      repr(Getter, "getter"),
      repr(Setter, "setter"),
      repr(Var, "var")
    ).flatten.mkString(",")
  }
}
object MethodFlags extends HasGenCodec[MethodFlags] {
  private[this] var currentFlag: Int = 1
  private[this] def nextFlag(): MethodFlags = {
    val flag = currentFlag
    currentFlag = currentFlag << 1
    new MethodFlags(flag)
  }

  final val Empty: MethodFlags = new MethodFlags(0)
  final val Abstract: MethodFlags = nextFlag()
  final val Final: MethodFlags = nextFlag()
  final val Lazy: MethodFlags = nextFlag()
  final val Getter: MethodFlags = nextFlag()
  final val Setter: MethodFlags = nextFlag()
  final val Var: MethodFlags = nextFlag()
}
