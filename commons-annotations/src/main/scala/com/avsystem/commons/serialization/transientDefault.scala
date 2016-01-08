package com.avsystem.commons
package serialization

import scala.annotation.StaticAnnotation

/**
  * If some case class field has default value, you can use this annotation on this field to instruct an
  * automatically derived [[GenCodec]] to not persist the value of that field if it's equal to the default value.
  *
  * For example:
  * {{{
  *   case class Something(str: String, @transientDefault int: Int = 42)
  *   object Something {
  *     implicit val codec = GenCodec.auto[Something]
  *   }
  * }}}
  *
  * `GenCodec.write(someOutput, Something("lol", 10))` would yield object `{"str": "lol", "int": 10}` but
  * `GenCodec.write(someOutput, Something("lol", 42))` would yield object `{"str": "lol"}` because the value of `int`
  * is the same as the default value.
  */
class transientDefault extends StaticAnnotation
