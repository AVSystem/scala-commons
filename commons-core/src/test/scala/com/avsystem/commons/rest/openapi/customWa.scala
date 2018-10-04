package com.avsystem.commons
package rest.openapi

import com.avsystem.commons.annotation.AnnotationAggregate
import com.avsystem.commons.serialization.whenAbsent

class customWa[+T](value: => T) extends AnnotationAggregate {
  @whenAbsent(value) type Implied
}
