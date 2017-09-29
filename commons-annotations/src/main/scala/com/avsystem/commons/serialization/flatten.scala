package com.avsystem.commons
package serialization

import scala.annotation.StaticAnnotation

/**
  * Changes the serialization format used by `GenCodec`s automatically derived for sealed hierarchies.
  * The format is changed from "nested" to "flat".
  *
  * {{{
  * @flatten sealed trait Value
  * case class Numeric(int: Int) extends Value
  * case class Textual(string: String) extends Value
  * object Value {
  *   implicit val codec: GenCodec[Value] = GenCodec.materialize[Value]
  * }
  * }}}
  *
  * Without [[flatten]] annotation, the "nested" format is used, e.g. when `Numeric(42)` would be encoded to JSON
  * as:
  *
  * {{{
  * {"Numeric": {"int": 42}}
  * }}}
  *
  * but when [[flatten]] annotation is applied on sealed trait/class, then it changes to:
  *
  * {{{
  * {"_case": "Numeric", "int": 42}
  * }}}
  */
class flatten extends StaticAnnotation
