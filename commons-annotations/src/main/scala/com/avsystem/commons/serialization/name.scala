package com.avsystem.commons
package serialization

import scala.annotation.StaticAnnotation

/**
  * Can be used on case class fields and classes in sealed hierarchy to instruct automatically derived [[GenCodec]]
  * to use particular name instead of just using parameter or class name.
  *
  * For example:
  * {{{
  *   case class Something(@name("dbname") paramname: Int)
  *   object Something {
  *     implicit codec = GenCodec.auto[Something]
  *   }
  * }}}
  *
  * `GenCodec.write(someOutput, Something(42))` would write an object `{"dbname": 42}` instead of `{"paramname": 42}`.
  */
class name(name: String) extends StaticAnnotation
