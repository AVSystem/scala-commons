package com.avsystem.commons
package mongo

import com.avsystem.commons.annotation.AnnotationAggregate
import com.avsystem.commons.serialization.{name, outOfOrder}

/**
  * Shortcut annotation for applying `@name("_id")` and `@outOfOrder` annotation on a case class field,
  * which is typical for MongoDB ID field.
  */
class mongoId extends AnnotationAggregate {
  @name("_id")
  @outOfOrder
  type Implied
}
