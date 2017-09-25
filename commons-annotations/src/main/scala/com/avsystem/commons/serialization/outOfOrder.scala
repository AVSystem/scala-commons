package com.avsystem.commons
package serialization

import scala.annotation.StaticAnnotation

/**
  * To be used in conjunction with [[flatten]].
  * It can be applied on one or more of case class fields in a sealed hierarchy to instruct the
  * auto-materialized `GenCodec` that this particular field may appear before `_case` field in the serialized format
  * during reading.
  *
  * {{{
  *   @flatten sealed trait Base
  *   case class FirstCase(@outOfOrder tag: String, int: Int) extends Base
  *   case class SecondCase(dbl: Double) extends Base
  *   object Base {
  *     implicit val codec: GenCodec[Base] = GenCodec.materialize[Base]
  *   }
  * }}}
  *
  * The following JSON (assuming this is the representation used) would correctly deserialize as
  * `FirstCase("someTag", 42)`:
  *
  * {{{
  *   {"tag": "someTag", "_case": "FirstCase", "int": 42}
  * }}}
  *
  * Field annotated with `@outOfOrder` annotation doesn't have to be present in every case class, but if it is present,
  * all case classes must annotate it and give it exactly the same type. The annotation may also be inherited, e.g.
  *
  * {{{
  *   @flatten sealed trait Base {
  *     @outOfOrder def tag: String
  *   }
  *   case class FirstCase(tag: String, int: Int) extends Base
  *   case class SecondCase(tag: String, dbl: Double) extends Base
  * }}}
  *
  */
class outOfOrder extends StaticAnnotation
