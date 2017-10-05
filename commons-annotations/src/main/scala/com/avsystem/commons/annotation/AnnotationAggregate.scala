package com.avsystem.commons
package annotation

import scala.annotation.StaticAnnotation

/**
  * When an annotation class extends this trait, annotation processing macros (e.g. for `GenCodec` materialization)
  * will look into annotations of the aggregating annotation itself and apply these annotations as if they were
  * applied directly on the same target as the aggregating annotation. Example:
  *
  * {{{
  *   import com.avsystem.commons.serialization._
  *
  *   @name("_id") @outOfOrder
  *   class mongoId extends AnnotationAggregate
  *
  *   case class SomeMongoEntity(@mongoId id: String, data: String)
  * }}}
  *
  * In the above example, applying `@mongoId` annotation on the `id` field has the same effect as if
  * annotations `@name("_id") @outOfOrder` were applied directly on that field.
  */
trait AnnotationAggregate extends StaticAnnotation
