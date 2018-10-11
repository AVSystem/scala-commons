package com.avsystem.commons
package misc

import com.avsystem.commons.annotation.AnnotationAggregate
import org.scalatest.FunSuite

import scala.annotation.StaticAnnotation

case class genann[T](value: T) extends StaticAnnotation
case class genagg[T](value: T) extends AnnotationAggregate {
  @genann(value) type Implied
}

@genagg(42)
class Subject

abstract class SelfAnnots(implicit val annots: SelfAnnotations[genann[_]])
@genagg(42) @genann("fuu") class Klass extends SelfAnnots
@genagg(42) @genann("fuu") object Objekt extends SelfAnnots

class AnnotationOfTest extends FunSuite {
  test("aggregate with generic") {
    assert(AnnotationOf.materialize[genann[Int], Subject].annot.value == 42)
  }

  test("self annotations") {
    assert(new Klass().annots.annots == List(genann(42), genann("fuu")))
    assert(Objekt.annots.annots == List(genann(42), genann("fuu")))
  }
}
