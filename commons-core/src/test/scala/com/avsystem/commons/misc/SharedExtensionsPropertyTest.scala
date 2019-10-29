package com.avsystem.commons
package misc

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSuite, Matchers}

class SharedExtensionsPropertyTest extends FunSuite with Matchers with GeneratorDrivenPropertyChecks {
  private val elementGen = Gen.alphaChar
  private val listGen = Gen.listOf(elementGen)
  private val listWithIndexGen = for {
    list <- Gen.nonEmptyListOf(elementGen)
    index <- Gen.choose(0, list.length - 1)
  } yield (list, index)
  private val listWithElementGen = for {
    list <- Gen.nonEmptyListOf(elementGen)
    elem <- Gen.oneOf(list)
  } yield (list, elem)

  test("indexOfOpt(elem) should return Opt.Empty if the element does not exist in the list") {
    forAll(listGen, elementGen) { (elemList, element) =>
      elemList.filterNot(_ == element).indexOfOpt(element) shouldEqual Opt.Empty
    }
  }

  test("indexOfOpt(elem) should not return Opt.Empty when the element exists in the list") {
    forAll(listWithElementGen) { case (elemList, elem) =>
      elemList.indexOfOpt(elem) should not equal Opt.Empty
    }
  }

  test("indexOfOpt(elem).getOrElse(-1) should behave identically to indexOf(elem)") {
    forAll(listGen, elementGen) { (elemList, element) =>
      elemList.indexOf(element) shouldEqual elemList.indexOfOpt(element).getOrElse(-1)
    }
  }

  test("indexOfOpt(elem, from).getOrElse(-1) should behave identically to indexOf(elem, from)") {
    forAll(listWithIndexGen, elementGen) { case ((elemList, idx), element) =>
      elemList.indexOf(element, idx) shouldEqual elemList.indexOfOpt(element, idx).getOrElse(-1)
    }
  }

  test("indexWhereOpt(elem).getOrElse(-1) should behave identically to indexWhere(elem)") {
    forAll(listGen, elementGen) { (elemList, element) =>
      elemList.indexWhere(_ == element) shouldEqual elemList.indexWhereOpt(_ == element).getOrElse(-1)
    }
  }

  test("indexWhereOpt(elem, from).getOrElse(-1) should behave identically to indexWhere(elem, from)") {
    forAll(listWithIndexGen, elementGen) { case ((elemList, idx), element) =>
      elemList.indexWhere(_ == element, idx) shouldEqual elemList.indexWhereOpt(_ == element, idx).getOrElse(-1)
    }
  }

}
