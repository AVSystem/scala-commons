package com.avsystem.commons
package misc

import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}

class SharedExtensionsPropertyTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks {
  implicit def shrinkNonEmpty[C[_] <: Iterable[_], T](implicit base: Shrink[C[T]]): Shrink[C[T]] =
    Shrink(base.shrink(_).filter(_.nonEmpty))

  private val elementGen = Gen.alphaChar
  private val listGen = Gen.listOf(elementGen)
  private val listWithElementGen = for {
    list <- Gen.nonEmptyListOf(elementGen)
    elem <- Gen.oneOf(list)
  } yield (list, elem)

  test("indexOfOpt(elem) should return Opt.Empty if the element does not occur in the list") {
    forAll(listGen, elementGen) { (elemList, element) =>
      elemList.filterNot(_ == element).indexOfOpt(element) shouldEqual Opt.Empty
    }
  }

  test("indexOfOpt(elem) should not return Opt.Empty when the element occurs in the list") {
    forAll(listWithElementGen) { case (elemList, elem) =>
      elemList.indexOfOpt(elem) should not equal Opt.Empty
    }
  }

  test("indexOfOpt(elem).getOrElse(-1) should behave identically to indexOf(elem)") {
    forAll(listGen, elementGen) { (elemList, element) =>
      elemList.indexOf(element) shouldEqual elemList.indexOfOpt(element).getOrElse(-1)
    }
  }

  test("indexWhereOpt(elem).getOrElse(-1) should behave identically to indexWhere(elem)") {
    forAll(listGen, elementGen) { (elemList, element) =>
      elemList.indexWhere(_ == element) shouldEqual elemList.indexWhereOpt(_ == element).getOrElse(-1)
    }
  }
}
