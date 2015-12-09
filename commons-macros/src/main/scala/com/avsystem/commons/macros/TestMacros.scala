package com.avsystem.commons
package macros

import scala.reflect.macros.blackbox

/**
  * Author: ghik
  * Created: 02/12/15.
  */
class TestMacros(val c: blackbox.Context) extends TypeClassDerivation {

  import c.universe._

  val TestObj = q"$CommonsPackage.macros.TypeClassDerivationTest"
  val SingletonTCObj = q"$TestObj.SingletonTC"
  val ApplyUnapplyTCObj = q"$TestObj.ApplyUnapplyTC"
  val SealedHierarchyTCObj = q"$TestObj.SealedHierarchyTC"
  val UnknownTCObj = q"$TestObj.UnknownTC"

  def typeClass(tpe: Type) = getType(tq"$TestObj.TC[$tpe]")
  def implementDeferredInstance(tpe: Type): Tree = q"new $TestObj.TC.Deferred[$tpe]"

  private def reifyRuntimeType(tpe: Type) =
    q"${c.reifyType(internal.gen.mkRuntimeUniverseRef, EmptyTree, tpe)}.tpe"

  def forSingleton(tpe: Type, singleValueTree: Tree): Tree =
    q"$SingletonTCObj[$tpe](${tpe.toString}, $singleValueTree)"

  def forApplyUnapply(tpe: Type, companion: Symbol, paramsWithInstances: List[(Symbol, Tree)]): Tree = {
    val deps = paramsWithInstances.map({ case (s, t) => q"(${s.name.toString}, $t)" })
    q"$ApplyUnapplyTCObj[$tpe](${tpe.toString}, List(..$deps))"
  }

  def forSealedHierarchy(tpe: Type, subtypesWithInstances: List[(Type, Tree)]): Tree = {
    val deps = subtypesWithInstances.map({ case (st, tree) => q"(${st.typeSymbol.name.toString}, $tree)" })
    q"$SealedHierarchyTCObj[$tpe](${tpe.toString}, List(..$deps))"
  }

  def forUnknown(tpe: Type): Tree =
    q"$UnknownTCObj[$tpe](${tpe.toString})"

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

  def testKnownSubtypes[T: c.WeakTypeTag, R: c.WeakTypeTag]: c.Tree = {
    val expectedResultTpe = knownSubtypes(weakTypeOf[T])
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
      .getOrElse(c.abort(c.enclosingPosition, s"Could not find unambiguous, matching pair of apply/unapply methods for $ttpe"))

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