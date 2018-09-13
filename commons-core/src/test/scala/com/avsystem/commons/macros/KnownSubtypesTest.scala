package com.avsystem.commons
package macros

import scala.language.experimental.macros

object KnownSubtypesTest {
  def testKnownSubtypes[T, R]: Nothing = macro com.avsystem.commons.macros.TestMacros.testKnownSubtypes[T, R]

  sealed trait Base
  case class Stuff(lol: Int) extends Base
  case object OtherStuff extends Base
  sealed trait SubHierarchy extends Base
  case class MoreStuff(str: String) extends SubHierarchy
  case object SubStuff extends SubHierarchy

  testKnownSubtypes[Int, Nothing]
  testKnownSubtypes[Option[String], (None.type, Some[String])]
  testKnownSubtypes[Base, (Stuff, OtherStuff.type, MoreStuff, SubStuff.type)]

  sealed trait Gadt[T]
  case class Something[T](t: T) extends Gadt[T]
  case class ListSomething[T](t: T) extends Gadt[List[T]]
  case class RandomGenericSomething[A](a: A) extends Gadt[Int]
  case object StringSomething extends Gadt[String]

  testKnownSubtypes[Gadt[Int], (Something[Int], RandomGenericSomething[_])]
  testKnownSubtypes[Gadt[String], (Something[String], StringSomething.type)]
  testKnownSubtypes[Gadt[List[Int]], (Something[List[Int]], ListSomething[Int])]

  sealed trait InvGadt[T]
  case class InvInt(lol: Int) extends InvGadt[Int]
  case class InvString(str: String) extends InvGadt[String]

  testKnownSubtypes[InvGadt[_], (InvInt, InvString)]
  testKnownSubtypes[InvGadt[Any], Unit]
  testKnownSubtypes[InvGadt[Nothing], Unit]

  sealed trait CovGadt[+T]
  case class CovInt(lol: Int) extends CovGadt[Int]
  case class CovString(str: String) extends CovGadt[String]

  testKnownSubtypes[CovGadt[_], (CovInt, CovString)]
  testKnownSubtypes[CovGadt[Any], (CovInt, CovString)]
  testKnownSubtypes[CovGadt[Nothing], Unit]

  sealed trait ContraGadt[-T]
  case class ContraInt(lol: Int) extends ContraGadt[Int]
  case class ContraString(str: String) extends ContraGadt[String]

  testKnownSubtypes[ContraGadt[_], (ContraInt, ContraString)]
  testKnownSubtypes[ContraGadt[Any], Unit]
  testKnownSubtypes[ContraGadt[Nothing], (ContraInt, ContraString)]

  sealed trait CovGeneric[+T]
  case class ListCovGeneric[+T](lt: List[T]) extends CovGeneric[List[T]]

  testKnownSubtypes[CovGeneric[List[Int]], ListCovGeneric[Int]]
  testKnownSubtypes[CovGeneric[Seq[Int]], ListCovGeneric[Int]]
  testKnownSubtypes[CovGeneric[Any], ListCovGeneric[Any]]

  object Outer {
    case object Inner extends Outer
  }
  sealed trait Outer

  testKnownSubtypes[Outer, Outer.Inner.type]

  sealed trait MemberedBase {
    type Elem
  }
  class MemberedCase extends MemberedBase {
    type Elem = String
  }
  class GenericMemberedCase[T] extends MemberedBase {
    type Elem = T
  }

  testKnownSubtypes[MemberedBase, (MemberedCase, GenericMemberedCase[_])]
  testKnownSubtypes[MemberedBase {type Elem = String}, (MemberedCase, GenericMemberedCase[String])]
  testKnownSubtypes[MemberedBase {type Elem = Int}, GenericMemberedCase[Int]]
}
