package com.avsystem.commons
package macros

import scala.language.experimental.macros

/**
  * Author: ghik
  * Created: 30/11/15.
  */
object KnownDirectSubtypesTest {
  def testKnownDirectSubtypes[T, R]: Nothing = macro com.avsystem.commons.macros.TestMacros.testKnownDirectSubtypes[T, R]

  sealed trait Base
  case class Stuff(lol: Int) extends Base

  case object OtherStuff extends Base
  sealed trait Gadt[T]
  case class Something[T](t: T) extends Gadt[T]
  case class ListSomething[T](t: T) extends Gadt[List[T]]

  case object StringSomething extends Gadt[String]
  sealed trait CovGadt[+T]
  case class CovInt(lol: Int) extends CovGadt[Int]
  case class CovString(str: String) extends CovGadt[String]

  testKnownDirectSubtypes[Int, Nothing]
  testKnownDirectSubtypes[Base, (Stuff, OtherStuff.type)]
  testKnownDirectSubtypes[Gadt[Int], Something[Int]]
  testKnownDirectSubtypes[Gadt[String], (Something[String], StringSomething.type)]
  testKnownDirectSubtypes[Gadt[List[Int]], (Something[List[Int]], ListSomething[Int])]
}
