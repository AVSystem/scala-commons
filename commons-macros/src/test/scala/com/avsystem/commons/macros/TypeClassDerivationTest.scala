package com.avsystem.commons
package macros

import org.scalatest.FunSuite

import scala.language.experimental.macros
import scala.reflect.runtime.{universe => ru}

/**
  * Author: ghik
  * Created: 04/12/15.
  */
object TypeClassDerivationTest {

  def autoDeriveFor[T]: TC[T] = macro com.avsystem.commons.macros.TestMacros.autoDerive[T]

  def typeRepr[T: ru.WeakTypeTag] = ru.weakTypeOf[T].toString

  trait TC[T] {
    def tpe: String

    def matches(pf: PartialFunction[TC[T], Any]): Boolean =
      pf.isDefinedAt(this)
  }
  case class SingletonTC[T](tpe: String, value: T) extends TC[T]
  case class ApplyUnapplyTC[T](tpe: String, subs: List[(String, TC[_])]) extends TC[T]
  case class SealedHierarchyTC[T](tpe: String, subs: List[(String, TC[_])]) extends TC[T]
  case class UnknownTC[T](tpe: String) extends TC[T]
  case class ForList[T](elementTc: TC[T]) extends TC[List[T]] {
    def tpe: String = s"List[${elementTc.tpe}]"
  }

  object TC {
    final class Deferred[T] extends DeferredInstance[TC[T]] with TC[T] {
      def tpe = underlying.tpe
      override def hashCode(): Int = System.identityHashCode(underlying)
      override def equals(obj: Any): Boolean = obj match {
        case df: Deferred[_] => underlying eq df.underlying
        case _ => false
      }
    }
    object Deferred {
      def apply[T](underlying: TC[T]) = {
        val res = new Deferred[T]
        res.underlying = underlying
        res
      }
    }
    implicit val forInt: TC[Int] = UnknownTC(typeRepr[Int])
    implicit val forString: TC[String] = UnknownTC(typeRepr[String])
    implicit def forList[T](implicit tct: TC[T]): TC[List[T]] = ForList(tct)
  }
}

class TypeClassDerivationTest extends FunSuite {

  import TypeClassDerivationTest._

  test("unknown test") {
    assert(autoDeriveFor[Int] == UnknownTC(typeRepr[Int]))
  }

  object SomeSingleton {
    implicit lazy val tc: TC[SomeSingleton.type] = autoDeriveFor[SomeSingleton.type]
  }

  test("singleton test") {
    assert(SomeSingleton.tc == SingletonTC(typeRepr[SomeSingleton.type], SomeSingleton))
  }

  case class Whatever(str: String, int: Int)
  object Whatever {
    implicit val tc: TC[Whatever] = autoDeriveFor[Whatever]
  }

  test("case class test") {
    assert(Whatever.tc == ApplyUnapplyTC(typeRepr[Whatever], List(
      ("str", TC.forString),
      ("int", TC.forInt)
    )))
  }

  sealed trait SealedRoot
  case class SealedCase(i: Int) extends SealedRoot
  case object SealedObj extends SealedRoot
  sealed trait SubRoot extends SealedRoot
  case class SubSealedCase(i: Int, w: Whatever) extends SubRoot

  object SealedRoot {
    implicit val tc: TC[SealedRoot] = autoDeriveFor[SealedRoot]
  }

  test("sealed hierarchy test") {
    assert(SealedRoot.tc == SealedHierarchyTC(typeRepr[SealedRoot], List(
      ("SealedCase", ApplyUnapplyTC(typeRepr[SealedCase], List(("i", TC.forInt)))),
      ("SealedObj", SingletonTC(typeRepr[SealedObj.type], SealedObj)),
      ("SubSealedCase", ApplyUnapplyTC(typeRepr[SubSealedCase], List(("i", TC.forInt), ("w", Whatever.tc))))
    )))
  }

  case class Recursive(str: String, next: Recursive)
  object Recursive {
    implicit val tc: TC[Recursive] = autoDeriveFor[Recursive]
  }

  test("recursive case class test") {
    assert(Recursive.tc == ApplyUnapplyTC(typeRepr[Recursive], List(
      ("str", TC.forString),
      ("next", TC.Deferred(Recursive.tc))
    )))
  }

  case class IndiRec(children: List[IndiRec])
  object IndiRec {
    implicit val tc: TC[IndiRec] = autoDeriveFor[IndiRec]
  }

  test("indirectly recursive case class test") {
    assert(IndiRec.tc == ApplyUnapplyTC(typeRepr[IndiRec], List(
      ("children", ForList(TC.Deferred(IndiRec.tc)))
    )))
  }

  sealed trait Tree[T]
  case class Leaf[T](value: T) extends Tree[T]
  case class Branch[T](left: Tree[T], right: Tree[T]) extends Tree[T]

  object Tree {
    implicit def tc[T: TC]: TC[Tree[T]] = autoDeriveFor[Tree[T]]
  }

  test("recursive GADT test") {
    def doTest[T](implicit tct: TC[T]): Unit = {
      val tc = Tree.tc[T]
      assert(tc == SealedHierarchyTC(typeRepr[Tree[T]], List(
        ("Leaf", ApplyUnapplyTC(typeRepr[Leaf[T]], List(("value", tct)))),
        ("Branch", ApplyUnapplyTC(typeRepr[Branch[T]], List(("left", TC.Deferred(tc)), ("right", TC.Deferred(tc)))))
      )))
    }
    doTest[String]
  }

}
