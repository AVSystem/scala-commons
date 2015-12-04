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

  testKnownDirectSubtypes[Int, Nothing]
  testKnownDirectSubtypes[Base, (Stuff, OtherStuff.type)]

  sealed trait Gadt[T]
  case class Something[T](t: T) extends Gadt[T]
  case class ListSomething[T](t: T) extends Gadt[List[T]]
  case object StringSomething extends Gadt[String]

  testKnownDirectSubtypes[Gadt[Int], Something[Int]]
  testKnownDirectSubtypes[Gadt[String], (Something[String], StringSomething.type)]
  testKnownDirectSubtypes[Gadt[List[Int]], (Something[List[Int]], ListSomething[Int])]

  sealed trait InvGadt[T]
  case class InvInt(lol: Int) extends InvGadt[Int]
  case class InvString(str: String) extends InvGadt[String]

  testKnownDirectSubtypes[InvGadt[_], (InvInt, InvString)]
  testKnownDirectSubtypes[InvGadt[Any], Unit]
  testKnownDirectSubtypes[InvGadt[Nothing], Unit]

  sealed trait CovGadt[+T]
  case class CovInt(lol: Int) extends CovGadt[Int]
  case class CovString(str: String) extends CovGadt[String]

  testKnownDirectSubtypes[CovGadt[_], (CovInt, CovString)]
  testKnownDirectSubtypes[CovGadt[Any], (CovInt, CovString)]
  testKnownDirectSubtypes[CovGadt[Nothing], Unit]

  sealed trait ContraGadt[-T]
  case class ContraInt(lol: Int) extends ContraGadt[Int]
  case class ContraString(str: String) extends ContraGadt[String]

  testKnownDirectSubtypes[ContraGadt[_], (ContraInt, ContraString)]
  testKnownDirectSubtypes[ContraGadt[Any], Unit]
  testKnownDirectSubtypes[ContraGadt[Nothing], (ContraInt, ContraString)]

  sealed trait CovGeneric[+T]
  case class ListCovGeneric[+T](lt: List[T]) extends CovGeneric[List[T]]

  testKnownDirectSubtypes[CovGeneric[List[Int]], ListCovGeneric[Int]]
  testKnownDirectSubtypes[CovGeneric[Seq[Int]], ListCovGeneric[Int]]
//  testKnownDirectSubtypes[CovGeneric[Any], ListCovGeneric[Any]]
}
