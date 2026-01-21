package com.avsystem.commons
package serialization

/**
 * To be used in conjunction with [[flatten]]. It can be applied on one or more of case class fields in a sealed
 * hierarchy to instruct the auto-materialized `GenCodec` that this particular field may appear before `_case` field in
 * the serialized format during reading.
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
 * The direct motivation for this annotation was the ability to use [[flatten]] on MongoDB entities with an `_id`
 * field. When reading entities from MongoDB, the `_id` field is always at the beginning of a BSON document and
 * therefore `GenCodec` must be able to read it before it knows which case class of sealed hierarchy is being
 * deserialized.
 */
class outOfOrder extends StaticAnnotation
