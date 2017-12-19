package com.avsystem.commons
package mongo

import com.avsystem.commons.annotation.AnnotationAggregate
import com.avsystem.commons.serialization.{name, outOfOrder}

/**
  * Shortcut annotation for applying `@name("_id")` and `@outOfOrder` annotation on a case class field,
  * which is typical for MongoDB ID field.
  */
@name("_id")
@outOfOrder
class mongoId extends AnnotationAggregate
