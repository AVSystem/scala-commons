package com.avsystem.commons
package misc

import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec, transparent}

/**
  * Typeclass that contains string representation of a concrete type. This representation should correctly parse
  * and typecheck when used as a type in Scala source code.
  *
  * Instances of `TypeString` are implicitly macro-materialized. The macro will fail if the type contains
  * references to local symbols, i.e. symbols that only exist in limited scope and cannot be referred to from
  * any place in source code. This includes type parameters, this-references to enclosing classes, etc.
  *
  * For example, the code below will NOT compile:
  * {{{
  *   def listTypeRepr[T]: String = TypeString.of[List[T]]
  * }}}
  * because `T` is a local symbol that only has meaning inside its own method. However, if you provide external
  * `TypeString` instance for `T`, the macro will pick it up and no longer complain:
  * {{{
  *   def listTypeRepr[T: TypeString]: String = TypeString.of[List[T]]
  * }}}
  * Then, `listTypeRepr[Int]` will produce a string `"List[Int]"`
  */
@transparent
case class TypeString[T](value: String) extends AnyVal
object TypeString {
  def of[T](implicit ts: TypeString[T]): String = ts.value

  implicit def materialize[T]: TypeString[T] = macro macros.misc.MiscMacros.typeString[T]

  implicit val keyCodec: GenKeyCodec[TypeString[_]] =
    GenKeyCodec.create[TypeString[Any]](TypeString(_), _.value).asInstanceOf[GenKeyCodec[TypeString[_]]]

  implicit val codec: GenCodec[TypeString[_]] =
    GenCodec.materialize[TypeString[Any]].asInstanceOf[GenCodec[TypeString[_]]]
}
