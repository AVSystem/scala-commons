package com.avsystem.commons
package macros

import scala.reflect.macros.blackbox

/**
  * Author: ghik
  * Created: 02/12/15.
  */
class TestMacros(val c: blackbox.Context) extends MacroCommons {

  import c.universe._

  def assertSameTypes(expected: Type, actual: Type): Unit = {
    if (!(expected =:= actual)) {
      c.abort(c.enclosingPosition, s"Types don't match, expected $expected")
    }
  }

  def testTreeForType(tpeRepr: c.Tree): c.Tree = {
    val Literal(Constant(repr)) = tpeRepr

    val Typed(_, tpt) = c.parse(s"(??? : $repr)")
    val tpe = getType(tpt)
    val newTree = treeForType(tpe)
    val newTpe = getType(newTree)

    assertSameTypes(tpe, newTpe)
    q"???"
  }

  def testKnownDirectSubtypes[T: c.WeakTypeTag, R: c.WeakTypeTag]: c.Tree = {
    val expectedResultTpe = knownDirectSubtypes(weakTypeOf[T])
      .map(types => getType(tq"(..$types)"))
      .getOrElse(typeOf[Nothing])

    assertSameTypes(expectedResultTpe, weakTypeOf[R])
    q"???"
  }

  val ApplierUnapplierCls = tq"$CommonsPackage.macros.ApplierUnapplier"

  def applierUnapplier[T: c.WeakTypeTag, F: c.WeakTypeTag]: c.Tree = {
    val ttpe = weakTypeOf[T]
    val ftpe = weakTypeOf[F]

    val ApplyUnapply(companion, params) = applyUnapplyFor(ttpe)

    val expectedTpe = params match {
      case Nil => typeOf[Unit]
      case List(single) => single.typeSignature
      case _ => getType(tq"(..${params.map(_.typeSignature)})")
    }
    assertSameTypes(expectedTpe, ftpe)

    val applyParams = params match {
      case List(single) => List(Ident(TermName("f")))
      case _ => params.indices.map(i => q"f.${TermName(s"_${i + 1}")}")
    }

    val unapplyResult = params match {
      case Nil => q"()"
      case _ => q"$companion.unapply(t).get"
    }

    q"""
       new $ApplierUnapplierCls[$ttpe,$ftpe] {
         def apply(f: $ftpe): $ttpe = $companion.apply(..$applyParams)
         def unapply(t: $ttpe): $ftpe = $unapplyResult
       }
     """
  }
}