package com.avsystem.commons
package annotation

import scala.annotation.StaticAnnotation

/**
  * Base trait for annotations which aggregate multiple other annotations. This way annotation aggregates
  * work like "annotation functions" - they are annotations that yield more annotations.
  *
  * In order to specify aggregated annotations, the class that extends `AnnotationAggregate` must
  * redefine the `Implied` dummy type member and apply the aggregated annotations on it. Macro engines
  * used in `GenCodec` materialization and RPC framework will automatically pick up these annotations.
  *
  * {{{
  *   import com.avsystem.commons.serialization._
  *
  *   class mongoId extends AnnotationAggregate {
  *     @name("_id") @outOfOrder
  *     type Implied
  *   }
  *
  *   case class SomeMongoEntity(@mongoId id: String, data: String)
  * }}}
  *
  * In the above example, applying `@mongoId` annotation on the `id` field has the same effect as if
  * annotations `@name("_id") @outOfOrder` were applied directly on that field.
  *
  * NOTE: thanks to the fact that aggregated annotations are applied on a type member
  * you can pass the arguments of original annotation to aggregated annotations, e.g.
  *
  * {{{
  *   class RPCNameAndDescription(name: String, description: String) extends AnnotationAggregate {
  *     @RPCName(name) // passing `name` to aggregated annotation
  *     type Implied
  *   }
  * }}}
  */
trait AnnotationAggregate extends StaticAnnotation {
  /**
    * Dummy type member meant to be redefined in order to have aggregated annotations applied on it.
    * These annotations will be automatically picked up by macro engines each time they encounter
    * the aggregating annotation itself.
    * Other than being an "anchor" for annotations, this type member has no actual meaning and there is no
    * reason to ever actually use it.
    * NOTE: a less weird solution would be to put aggregated annotations on the
    * aggregating annotation class itself, but this would make it impossible to access the arguments
    * of aggregating annotation in aggregated annotations.
    */
  type Implied
}
