package com.avsystem.commons
package serialization

import com.avsystem.commons.serialization.AutoGenCodecTest.ValueClass

/**
  * Author: ghik
  * Created: 02/02/16.
  */
object AutoGenCodecTest {
  case class ValueClass(str: String) extends AnyVal
}

class AutoGenCodecTest extends CodecTestBase {
  object SomeObject

  test("object test") {
    testAutoWriteRead(SomeObject, Map())
  }

  case class NoArgCaseClass()

  test("no arg case class test") {
    testAutoWriteRead(NoArgCaseClass(), Map())
  }

  case class SingleArgCaseClass(str: String)

  test("single arg case class test") {
    testAutoWriteRead(SingleArgCaseClass("something"), Map("str" -> "something"))
  }

  @transparent
  case class TransparentWrapper(str: String)

  test("transparent wrapper test") {
    testAutoWriteRead(TransparentWrapper("something"), "something")
  }

  case class SomeCaseClass(@name("some.str") str: String, intList: List[Int])

  test("case class test") {
    testAutoWriteRead(SomeCaseClass("dafuq", List(1, 2, 3)),
      Map("some.str" -> "dafuq", "intList" -> List(1, 2, 3))
    )
  }

  case class HasDefaults(@transientDefault int: Int = 42, str: String)

  test("case class with default values test") {
    testAutoWriteRead(HasDefaults(str = "lol"), Map("str" -> "lol"))
    testAutoWriteRead(HasDefaults(43, "lol"), Map("int" -> 43, "str" -> "lol"))
    testAutoWriteRead(HasDefaults(str = null), Map("str" -> null))
  }

  case class Node[T](value: T, children: List[Node[T]] = Nil)

  test("recursive generic case class test") {
    testAutoWriteRead(
      Node(123, List(
        Node(42, List(
          Node(52),
          Node(53)
        )),
        Node(43)
      )),
      Map[String, Any]("value" -> 123, "children" -> List(
        Map[String, Any]("value" -> 42, "children" -> List(
          Map[String, Any]("value" -> 52, "children" -> List()),
          Map[String, Any]("value" -> 53, "children" -> List())
        )),
        Map[String, Any]("value" -> 43, "children" -> List())
      ))
    )
  }

  test("value class test") {
    testAutoWriteRead(ValueClass("costam"), Map("str" -> "costam"))
  }

  sealed trait Tree[T]
  case class Leaf[T](value: T) extends Tree[T]
  case class Branch[T](left: Tree[T], right: Tree[T]) extends Tree[T]

  test("recursive gadt test") {
    testAutoWriteRead[Tree[Int]](
      Branch(
        Leaf(1),
        Branch(
          Leaf(2),
          Leaf(3)
        )
      ),
      Map("Branch" -> Map(
        "left" -> Map("Leaf" -> Map("value" -> 1)),
        "right" -> Map("Branch" -> Map(
          "left" -> Map("Leaf" -> Map("value" -> 2)),
          "right" -> Map("Leaf" -> Map("value" -> 3))
        ))
      ))
    )
  }

  sealed trait Enumz
  object Enumz {
    @name("Primary")
    case object First extends Enumz
    case object Second extends Enumz
    case object Third extends Enumz
  }

  test("sealed enum test") {
    testAutoWriteRead[Enumz](Enumz.First, Map("Primary" -> Map()))
  }


  case class Bottom(str: String)
  case class Middle(bottom: Bottom)
  case class Top(middle: Middle)
  object Top {
    implicit val codec: GenCodec[Top] = GenCodec.recursiveAuto[Top]
  }
}
