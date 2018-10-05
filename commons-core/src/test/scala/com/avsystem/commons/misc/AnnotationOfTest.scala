package com.avsystem.commons
package misc

import com.avsystem.commons.annotation.AnnotationAggregate
import org.scalatest.FunSuite

import scala.annotation.StaticAnnotation

class genann[T](val value: T) extends StaticAnnotation
class genagg[T](value: T) extends AnnotationAggregate {
  @genann(value) type Implied
}

@genagg(42)
class Subject

class AnnotationOfTest extends FunSuite {
  test("aggregate with generic") {
    assert(AnnotationOf.materialize[genann[Int], Subject].annot.value == 42)
  }
}
