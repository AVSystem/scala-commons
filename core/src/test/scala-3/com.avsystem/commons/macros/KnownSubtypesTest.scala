package com.avsystem.commons
package macros

class KnownSubtypesTest[A, B <: AnyRef, C <: Ordered[C]] {
  def testKnownSubtypes[T, R]: Nothing = com.avsystem.commons.macros.TestMacros.testKnownSubtypes[T, R]

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

  testKnownSubtypes[Gadt[Int], (Something[Int], RandomGenericSomething[?], StringSomething.type)]
  testKnownSubtypes[Gadt[String], (Something[String], StringSomething.type)]
  testKnownSubtypes[Gadt[List[Int]], (Something[List[Int]], ListSomething[Int], StringSomething.type)]
  testKnownSubtypes[Gadt[A], (Something[A], ListSomething[?], RandomGenericSomething[?], StringSomething.type)]

  sealed trait InvGadt[T]
  case class InvInt(lol: Int) extends InvGadt[Int]
  case class InvString(str: String) extends InvGadt[String]
  case class InvGen[T](t: T) extends InvGadt[T]
  case class InvBounded[T <: AnyRef](t: T) extends InvGadt[T]
  case class InvRecBounded[T <: Ordered[T]](t: T) extends InvGadt[T]

  testKnownSubtypes[InvGadt[?], (InvInt, InvString, InvGen[?], InvBounded[?], InvRecBounded[?])]
  testKnownSubtypes[InvGadt[
    ? <: String,
  ], (InvInt, InvString, InvGen[? <: String], InvBounded[? <: String], InvRecBounded[? <: String])]
  // testKnownSubtypes[InvGadt[Set[_]], (InvInt, InvString, InvGen[Set[_]], InvBounded[Set[_]], InvRecBounded[Set[_]])]
  // testKnownSubtypes[
  //   InvGadt[Set[?]],
  //   (
  //     InvInt,
  //     InvString,
  //     InvGen[Set[?]],
  //     InvBounded[Set[?]],
  //     InvRecBounded[Set[?]],
  //   ),
  // ]
  // testKnownSubtypes[InvGadt[Any], (InvInt, InvString, InvGen[Any], InvBounded[String], InvRecBounded[String])]
  // testKnownSubtypes[
  //   InvGadt[T] forSome { type T <: Ordered[T] },
  //   (
  //     InvInt,
  //     InvString,
  //     InvGen[T] forSome { type T <: Ordered[T] },
  //     InvBounded[T] forSome { type T <: Ordered[T] },
  //     InvRecBounded[T] forSome { type T <: Ordered[T] },
  //   ),
  // ]
  // testKnownSubtypes[InvGadt[Nothing], (InvInt, InvString, InvGen[Nothing], InvBounded[Nothing], InvRecBounded[Nothing])]
  // testKnownSubtypes[InvGadt[A], (InvInt, InvString, InvGen[A], InvBounded[A], InvRecBounded[A])]
  // testKnownSubtypes[InvGadt[B], (InvInt, InvString, InvGen[B], InvBounded[B], InvRecBounded[B])]
  // testKnownSubtypes[InvGadt[C], (InvInt, InvString, InvGen[C], InvBounded[C], InvRecBounded[C])]
  // testKnownSubtypes[InvGadt[String], (InvInt, InvString, InvGen[String], InvBounded[String], InvRecBounded[String])]

  sealed trait CovGadt[+T]
  case class CovInt(lol: Int) extends CovGadt[Int]
  case class CovString(str: String) extends CovGadt[String]
  case class CovGen[+T](t: T) extends CovGadt[T]
  case class CovInvGen[T](t: T) extends CovGadt[T]

  testKnownSubtypes[CovGadt[?], (CovInt, CovString, CovGen[Any], CovInvGen[?])]
  testKnownSubtypes[CovGadt[Any], (CovInt, CovString, CovGen[Any], CovInvGen[?])]
  testKnownSubtypes[CovGadt[Nothing], (CovInt, CovString, CovGen[Nothing], CovInvGen[? <: Nothing])]
  testKnownSubtypes[CovGadt[A], (CovInt, CovString, CovGen[A], CovInvGen[? <: A])]

  sealed trait ContraGadt[-T]
  case class ContraInt(lol: Int) extends ContraGadt[Int]
  case class ContraString(str: String) extends ContraGadt[String]
  case class ContraGen[-T]() extends ContraGadt[T]
  case class ContraInvGen[T](t: T) extends ContraGadt[T]

  // ContraInvGen case unnecessarily bloated but that shouldn't be a problem
  testKnownSubtypes[ContraGadt[?], (ContraInt, ContraString, ContraGen[?], ContraInvGen[?])]
  testKnownSubtypes[ContraGadt[Any], (ContraInt, ContraString, ContraGen[Any], ContraInvGen[? >: Any])]
  testKnownSubtypes[ContraGadt[Nothing], (ContraInt, ContraString, ContraGen[Nothing], ContraInvGen[?])]
  testKnownSubtypes[ContraGadt[A], (ContraInt, ContraString, ContraGen[A], ContraInvGen[? >: A])]

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

  testKnownSubtypes[MemberedBase, (MemberedCase, GenericMemberedCase[?])]
  testKnownSubtypes[MemberedBase { type Elem = String }, (MemberedCase, GenericMemberedCase[String])]
  testKnownSubtypes[MemberedBase { type Elem = Int }, (MemberedCase, GenericMemberedCase[Int])]
}
