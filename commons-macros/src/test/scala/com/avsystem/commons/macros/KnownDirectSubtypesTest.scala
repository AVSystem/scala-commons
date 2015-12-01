package com.avsystem.commons
package macros

import scala.language.experimental.macros

sealed trait Base
case class Stuff(lol: Int) extends Base
case object OtherStuff extends Base

sealed trait Gadt[T]
case class Something[T](t: T) extends Gadt[T]
case class ListSomething[T](t: T) extends Gadt[List[T]]
case object StringSomething extends Gadt[String]

/**
  * Author: ghik
  * Created: 30/11/15.
  */
object KnownDirectSubtypesTest {
  def testKnownDirectSubtypes[T, R]: Nothing = macro com.avsystem.commons.macros.TestMacros.testKnownDirectSubtypes[T, R]

  testKnownDirectSubtypes[Int, Nothing]
  testKnownDirectSubtypes[Base, (Stuff, OtherStuff.type)]
  testKnownDirectSubtypes[Gadt[List[T]] forSome {type T}, (Something[List[T]] forSome {type T}, ListSomething[_])]
  testKnownDirectSubtypes[Gadt[Int], Something[Int]]
  testKnownDirectSubtypes[Gadt[String], (Something[String], StringSomething.type)]
  testKnownDirectSubtypes[Gadt[List[Int]], (Something[List[Int]], ListSomething[Int])]
}
