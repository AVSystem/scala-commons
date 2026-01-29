package com.avsystem.commons
package serialization

import org.scalatest.funsuite.AnyFunSuite

case class CodeSizeTester00(
  int: Int,
  string: String,
  double: Double,
  map: Map[String, List[Boolean]],
  people: Set[Person],
)
object CodeSizeTester00 {
  given GenCodec[CodeSizeTester00] = GenCodec.materialize[CodeSizeTester00]
}

case class CodeSizeTester01(
  int: Int,
  string: String,
  double: Double,
  map: Map[String, List[Boolean]],
  people: Set[Person],
)
object CodeSizeTester01 {
  given GenCodec[CodeSizeTester01] = GenCodec.materialize
}

case class CodeSizeTester02(
  int: Int,
  string: String,
  double: Double,
  map: Map[String, List[Boolean]],
  people: Set[Person],
)
object CodeSizeTester02 {
  given GenCodec[CodeSizeTester02] = GenCodec.materialize
}

case class CodeSizeTester03(
  int: Int,
  string: String,
  double: Double,
  map: Map[String, List[Boolean]],
  people: Set[Person],
)
object CodeSizeTester03 {
  given GenCodec[CodeSizeTester03] = GenCodec.materialize
}

case class CodeSizeTester04(
  int: Int,
  string: String,
  double: Double,
  map: Map[String, List[Boolean]],
  people: Set[Person],
)
object CodeSizeTester04 {
  given GenCodec[CodeSizeTester04] = GenCodec.materialize
}

case class CodeSizeTester05(
  int: Int,
  string: String,
  double: Double,
  map: Map[String, List[Boolean]],
  people: Set[Person],
)
object CodeSizeTester05 {
  given GenCodec[CodeSizeTester05] = GenCodec.materialize
}

case class CodeSizeTester06(
  int: Int,
  string: String,
  double: Double,
  map: Map[String, List[Boolean]],
  people: Set[Person],
)
object CodeSizeTester06 {
  given GenCodec[CodeSizeTester06] = GenCodec.materialize
}

case class CodeSizeTester07(
  int: Int,
  string: String,
  double: Double,
  map: Map[String, List[Boolean]],
  people: Set[Person],
)
object CodeSizeTester07 {
  given GenCodec[CodeSizeTester07] = GenCodec.materialize
}

case class CodeSizeTester08(
  int: Int,
  string: String,
  double: Double,
  map: Map[String, List[Boolean]],
  people: Set[Person],
)
object CodeSizeTester08 {
  given GenCodec[CodeSizeTester08] = GenCodec.materialize
}

case class CodeSizeTester09(
  int: Int,
  string: String,
  double: Double,
  map: Map[String, List[Boolean]],
  people: Set[Person],
)
object CodeSizeTester09 {
  given GenCodec[CodeSizeTester09] = GenCodec.materialize
}

case class Person(name: String, birthYear: Int, planet: String = "Earth")
object Person {
  given GenCodec[Person] = GenCodec.materialize
}

class CodeSizeTester extends AnyFunSuite {
  ignore("fake test to see how much JS is generated") {
    println(GenCodec[CodeSizeTester00].write(null.asInstanceOf[Output], null.asInstanceOf[CodeSizeTester00]))
//    println(CodeSizeTester01.codec.write(null, null))

    println(GenCodec[CodeSizeTester00].read(null.asInstanceOf[Input]))
//    println(CodeSizeTester01.codec.read(null))
  }
}
