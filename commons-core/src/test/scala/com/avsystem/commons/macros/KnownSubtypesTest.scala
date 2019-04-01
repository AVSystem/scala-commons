package com.avsystem.commons
package macros

import scala.language.experimental.macros

class KnownSubtypesTest[A, B <: AnyRef] {
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
  testKnownSubtypes[Option[A], (None.type, Some[A])]

  sealed trait Gadt[T]
  case class Something[T](t: T) extends Gadt[T]
  case class ListSomething[T](t: T) extends Gadt[List[T]]
  case class RandomGenericSomething[T](t: T) extends Gadt[Int]
  case object StringSomething extends Gadt[String]

  testKnownSubtypes[Gadt[Int], (Something[Int], RandomGenericSomething[_])]
  testKnownSubtypes[Gadt[String], (Something[String], StringSomething.type)]
  testKnownSubtypes[Gadt[List[Int]], (Something[List[Int]], ListSomething[Int])]
  testKnownSubtypes[Gadt[A], (Something[A], ListSomething[_], RandomGenericSomething[_], StringSomething.type)]

  sealed trait InvGadt[T]
  case class InvInt(lol: Int) extends InvGadt[Int]
  case class InvString(str: String) extends InvGadt[String]
  case class InvGen[T](t: T) extends InvGadt[T]
  case class InvBounded[T <: AnyRef](t: T) extends InvGadt[T]

  testKnownSubtypes[InvGadt[_], (InvInt, InvString, InvGen[_], InvBounded[_ <: AnyRef])]
  testKnownSubtypes[InvGadt[_ <: String], (InvString, InvGen[_ <: String], InvBounded[_ <: String])]
  testKnownSubtypes[InvGadt[Set[_]], (InvGen[Set[_]], InvBounded[Set[_]])]
  testKnownSubtypes[InvGadt[Set[X]] forSome {type X},
    (InvGen[Set[X]] forSome {type X}, InvBounded[Set[X]] forSome {type X})]
  testKnownSubtypes[InvGadt[Any], InvGen[Any]]
  testKnownSubtypes[InvGadt[Nothing], (InvGen[Nothing], InvBounded[Nothing])]
  testKnownSubtypes[InvGadt[A], (InvInt, InvString, InvGen[A], InvBounded[_ <: AnyRef])]
  testKnownSubtypes[InvGadt[B], (InvString, InvGen[B], InvBounded[B])]

  sealed trait CovGadt[+T]
  case class CovInt(lol: Int) extends CovGadt[Int]
  case class CovString(str: String) extends CovGadt[String]
  case class CovGen[+T](t: T) extends CovGadt[T]
  case class CovInvGen[T](t: T) extends CovGadt[T]

  testKnownSubtypes[CovGadt[_], (CovInt, CovString, CovGen[Any], CovInvGen[_])]
  testKnownSubtypes[CovGadt[Any], (CovInt, CovString, CovGen[Any], CovInvGen[_])]
  testKnownSubtypes[CovGadt[Nothing], (CovGen[Nothing], CovInvGen[Nothing])]
  testKnownSubtypes[CovGadt[A], (CovInt, CovString, CovGen[A], CovInvGen[_ <: A])]

  sealed trait ContraGadt[-T]
  case class ContraInt(lol: Int) extends ContraGadt[Int]
  case class ContraString(str: String) extends ContraGadt[String]
  case class ContraGen[-T]() extends ContraGadt[T]
  case class ContraInvGen[T](t: T) extends ContraGadt[T]

  //type inferred for ContraInvGen is unnecessarily bloated but it's an unlikely corner case
  testKnownSubtypes[ContraGadt[_],
    (ContraInt, ContraString, ContraGen[_], ContraInvGen[X] forSome {type Y; type X >: Y})]
  testKnownSubtypes[ContraGadt[Any], (ContraGen[Any], ContraInvGen[Any])]
  testKnownSubtypes[ContraGadt[Nothing], (ContraInt, ContraString, ContraGen[_], ContraInvGen[_])]
  testKnownSubtypes[ContraGadt[A], (ContraInt, ContraString, ContraGen[A], ContraInvGen[_ >: A])]

  sealed trait CovGeneric[+T]
  case class ListCovGeneric[+T](lt: List[T]) extends CovGeneric[List[T]]

  testKnownSubtypes[CovGeneric[List[Int]], ListCovGeneric[Int]]
  testKnownSubtypes[CovGeneric[Seq[Int]], ListCovGeneric[Int]]
  testKnownSubtypes[CovGeneric[Any], ListCovGeneric[Any]]
  testKnownSubtypes[CovGeneric[List[A]], ListCovGeneric[A]]
  testKnownSubtypes[CovGeneric[A], ListCovGeneric[Any]]

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
