package com.avsystem.commons
package macros

import com.avsystem.commons.misc.{JavaClassName, TypeString}
import org.scalactic.source.Position
import org.scalatest.FunSuite

object JavaClassNameTest {
  class Inner {
    class MoreInner {
      class SuperInner
    }
  }
  object Inner {
    class EvenInner
    object EvenInner
  }
}

class JavaClassNameTest extends FunSuite {
  def test[T: ClassTag : JavaClassName : TypeString](implicit pos: Position): Unit =
    test(TypeString.of[T])(assert(JavaClassName.of[T] == classTag[T].runtimeClass.getName))

  test[Any]
  test[AnyRef]
  test[AnyVal]
  test[Unit]
  test[Boolean]
  test[Char]
  test[Byte]
  test[Short]
  test[Int]
  test[Long]
  test[Float]
  test[Double]
  test[String]
  test[Nothing]
  test[Array[Unit]]
  test[Array[Boolean]]
  test[Array[Char]]
  test[Array[Byte]]
  test[Array[Short]]
  test[Array[Int]]
  test[Array[Long]]
  test[Array[Float]]
  test[Array[Double]]
  test[Array[String]]
  test[Array[Nothing]]
  test[JavaClassNameTest]
  test[JavaClassNameTest.type]
  test[JavaClassNameTest.Inner]
  test[JavaClassNameTest.Inner#MoreInner]
  test[JavaClassNameTest.Inner#MoreInner#SuperInner]
  test[JavaClassNameTest.Inner.type]
  test[JavaClassNameTest.Inner.EvenInner]
  test[JavaClassNameTest.Inner.EvenInner.type]
}
