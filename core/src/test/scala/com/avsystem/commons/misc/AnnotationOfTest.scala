package com.avsystem.commons
package misc

import com.avsystem.commons.annotation.AnnotationAggregate
import org.scalatest.funsuite.AnyFunSuite

case class genann[T](value: T) extends StaticAnnotation
case class genagg[T](value: T) extends AnnotationAggregate {
  @genann(value)
  final def aggregated: List[StaticAnnotation] = reifyAggregated
}

case class varargann(value: Int, varValue: String*) extends StaticAnnotation

@genagg(42)
@varargann(1, "test", "foo", "bar")
class Subject

abstract class SelfAnnots(implicit val annots: SelfAnnotations[genann[_]])

@genagg(42)
@genann("fuu")
class Klass extends SelfAnnots

@genagg(42)
@genann("fuu")
object Objekt extends SelfAnnots

class AnnotationOfTest extends AnyFunSuite {
  test("aggregate with generic") {
    assert(AnnotationOf.materialize[genann[Int], Subject].annot.value == 42)
  }

  test("self annotations") {
    assert(new Klass().annots.annots == List(genann(42), genann("fuu")))
    assert(Objekt.annots.annots == List(genann(42), genann("fuu")))
  }

  test("annotation with varargs") {
    val annot = AnnotationOf.materialize[varargann, Subject].annot
    assert(annot.value == 1)
    assert(annot.varValue == Seq("test", "foo", "bar"))
  }
}
