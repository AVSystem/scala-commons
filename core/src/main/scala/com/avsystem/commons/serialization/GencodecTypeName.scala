package com.avsystem.commons
package serialization

import com.avsystem.commons.annotation.explicitGenerics

/**
  * Typeclass holding name of a type that will be used in [[GenCodec]] serialization
  *
  * @see [[com.avsystem.commons.serialization.GenCodecUtils.codecTypeName]]
  */
final class GencodecTypeName[T](val name: String)
object GencodecTypeName {
  def apply[T](implicit tpeName: GencodecTypeName[T]): GencodecTypeName[T] = tpeName

  implicit def materialize[T]: GencodecTypeName[T] =
    macro com.avsystem.commons.macros.serialization.GenCodecUtilMacros.codecTypeName[T]
}

object GenCodecUtils {
  /**
    * Allows to extract case class name that will be used in [[GenCodec]] serialization format when dealing with sealed
    * hierarchies.
    *
    * {{{
    *   @name("SomethingElse")
    *   final case class Example(something: String)
    *   object Example extends HasGenCodec[Example]
    *
    *   GenCodecUtils.codecTypeName[Example] // "SomethingElse"
    * }}}
    *
    * @return name of case class, possibility adjusted by [[com.avsystem.commons.serialization.name]] annotation
    */
  @explicitGenerics
  def codecTypeName[T]: String =
    macro com.avsystem.commons.macros.serialization.GenCodecUtilMacros.codecTypeNameRaw[T]

  /**
    * Allows to extract case class field name that will be used in [[GenCodec]] serialization format
    * {{{
    *   final case class Example(something: String, @name("otherName") somethingElse: Int)
    *   object Example extends HasGenCodec[Example]
    *
    *   GenCodecUtils.codecFieldName[Example](_.somethingElse) // "otherName"
    * }}}
    *
    * @return name of case class field, possibility adjusted by [[com.avsystem.commons.serialization.name]] annotation
    */
  def codecFieldName[T](accessor: T => Any): String =
    macro com.avsystem.commons.macros.serialization.GenCodecUtilMacros.codecFieldName[T]

  /**
    * @return number of sealed hierarchy subclasses or `0` if specified type is not a hierarchy
    */
  @explicitGenerics
  def knownSubtypesCount[T]: Int =
    macro com.avsystem.commons.macros.serialization.GenCodecUtilMacros.knownSubtypesCount[T]
}
