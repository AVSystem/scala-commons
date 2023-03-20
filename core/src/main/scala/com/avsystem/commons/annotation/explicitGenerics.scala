package com.avsystem.commons
package annotation

/**
  * When applied on generic method, requires that all the type parameters are given explicitly
  * (cannot be inferred by the compiler). This is meant primarily for methods whose generics cannot be
  * inferred from method arguments. Requiring that the programmer specifies them explicitly is a protection
  * against the compiler inferring `Nothing` or `Null`.
  * {{{
  *   @explicitGenerics
  *   def readJson[T: GenCodec](json: String): T = ...
  *
  *   // raise error, because otherwise we have a hidden bug - the compiler infers `Nothing` in place of `T`
  *   val x: MyType = readJson("{}")
  *   // ok
  *   val x = readJson[MyType]("{}")
  * }}}
  */
class explicitGenerics extends StaticAnnotation
