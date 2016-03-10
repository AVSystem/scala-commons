package com.avsystem.commons
package macros

import com.avsystem.commons.derivation.DeferredInstance
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

  class DefVal(v: => Any) {
    lazy val value = v
    override def equals(other: Any) = other match {
      case otherDef: DefVal => value == otherDef.value
      case _ => false
    }
    override def hashCode() = value.hashCode
  }
  object DefVal {
    def apply(value: => Any) = new DefVal(value)
    def unapply(ddef: DefVal): Option[Any] = Some(ddef.value)
  }

  trait TC[T] {
    def tpe: String

    def matches(pf: PartialFunction[TC[T], Any]): Boolean =
      pf.isDefinedAt(this)
  }
  case class SingletonTC[T](tpe: String, value: T) extends TC[T]
  case class ApplyUnapplyTC[T](tpe: String, subs: List[(String, TC[_], Option[DefVal])]) extends TC[T]
  case class SealedHierarchyTC[T](tpe: String, subs: List[(String, TC[_])]) extends TC[T]
  case class UnknownTC[T](tpe: String) extends TC[T]
  case class ForList[T](elementTc: TC[T]) extends TC[List[T]] {
    def tpe: String = s"List[${elementTc.tpe}]"
  }

  object TC {
    case class Auto[T](tc: TC[T]) extends AnyVal
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

  case class Whatever(str: String, int: Int = 42)
  object Whatever {
    implicit val tc: TC[Whatever] = autoDeriveFor[Whatever]
  }

  test("case class test") {
    assert(Whatever.tc == ApplyUnapplyTC(typeRepr[Whatever], List(
      ("str", TC.forString, None),
      ("int", TC.forInt, Some(DefVal(42)))
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
      ("SealedCase", ApplyUnapplyTC(typeRepr[SealedCase], List(("i", TC.forInt, None)))),
      ("SealedObj", SingletonTC(typeRepr[SealedObj.type], SealedObj)),
      ("SubSealedCase", ApplyUnapplyTC(typeRepr[SubSealedCase], List(("i", TC.forInt, None), ("w", Whatever.tc, None))))
    )))
  }

  case class Recursive(str: String, next: Recursive)
  object Recursive {
    implicit val tc: TC[Recursive] = autoDeriveFor[Recursive]
  }

  test("recursive case class test") {
    assert(Recursive.tc == ApplyUnapplyTC(typeRepr[Recursive], List(
      ("str", TC.forString, None),
      ("next", TC.Deferred(Recursive.tc), None)
    )))
  }

  case class IndiRec(children: List[IndiRec])
  object IndiRec {
    implicit val tc: TC[IndiRec] = autoDeriveFor[IndiRec]
  }

  test("indirectly recursive case class test") {
    assert(IndiRec.tc == ApplyUnapplyTC(typeRepr[IndiRec], List(
      ("children", ForList(TC.Deferred(IndiRec.tc)), None)
    )))
  }

  sealed trait Tree[T]
  case class Leaf[T](value: T) extends Tree[T]
  case class Branch[T](left: Tree[T], right: Tree[T]) extends Tree[T]

  object Tree {
    implicit def tc[A: TC]: TC[Tree[A]] = autoDeriveFor[Tree[A]]
  }

  test("recursive GADT test") {
    def doTest[A](implicit tct: TC[A]): Unit = {
      val tc = Tree.tc[A]
      assert(tc == SealedHierarchyTC(typeRepr[Tree[A]], List(
        ("Leaf", ApplyUnapplyTC(typeRepr[Leaf[A]], List(("value", tct, None)))),
        ("Branch", ApplyUnapplyTC(typeRepr[Branch[A]], List(
          ("left", TC.Deferred(tc), None),
          ("right", TC.Deferred(tc), None)
        )))
      )))
    }
    doTest[String]
  }

}
