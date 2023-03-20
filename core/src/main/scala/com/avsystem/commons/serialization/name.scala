package com.avsystem.commons
package serialization

import com.avsystem.commons.annotation.NotInheritedFromSealedTypes

/**
  * Can be used on case class fields and classes in sealed hierarchy to instruct automatically derived `GenCodec`
  * to use particular name instead of just using parameter or class name.
  *
  * For example:
  * {{{
  *   sealed trait Base
  *   @name("STH")
  *   case class Something(@name("dbname") paramname: Int) extends Base
  *   object Base {
  *     implicit codec = GenCodec.auto[Base]
  *   }
  * }}}
  *
  * `GenCodec.write[Base](someOutput, Something(42))` would write an object
  * `{"STH": {"dbname": 42}}` instead of `{"Something": {"paramname": 42}}`.
  *
  * NOTE: `@name` annotation may be defined on any level of inheritance hierarchy.
  * For instance, if a case class field overrides a method of some base trait, the `@name` annotation may
  * be used on that method and will affect the case class field.
  */
class name(val name: String) extends StaticAnnotation with NotInheritedFromSealedTypes

