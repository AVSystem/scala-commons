package com.avsystem.commons
package macros

import scala.reflect.macros.blackbox

class TestMacros(val c: blackbox.Context) extends TypeClassDerivation {

  import c.universe._

  val TestObj = q"$CommonsPackage.macros.TypeClassDerivationTest"
  val SingletonTCObj = q"$TestObj.SingletonTC"
  val ApplyUnapplyTCObj = q"$TestObj.ApplyUnapplyTC"
  val SealedHierarchyTCObj = q"$TestObj.SealedHierarchyTC"
  val UnknownTCObj = q"$TestObj.UnknownTC"
  val DefValObj = q"$TestObj.DefVal"

  def typeClass = tq"$TestObj.TC"
  def typeClassName = "TC"
  def wrapInAuto(tree: Tree) = q"$TestObj.TC.Auto($tree)"
  def implementDeferredInstance(tpe: Type): Tree = q"new $TestObj.TC.Deferred[$tpe]"

  def forSingleton(tpe: Type, singleValueTree: Tree): Tree =
    q"$SingletonTCObj[$tpe](${tpe.toString}, $singleValueTree)"

  def forApplyUnapply(tpe: Type, apply: Symbol, unapply: Symbol, params: List[ApplyParam]): Tree = {
    val deps = params.map { case ApplyParam(s, dv, t) =>
      val defaultValueOpt = if (dv == EmptyTree) q"None" else q"Some($DefValObj($dv))"
      q"(${s.name.toString}, $t, $defaultValueOpt)"
    }
    q"$ApplyUnapplyTCObj[$tpe](${tpe.toString}, List(..$deps))"
  }

  def forSealedHierarchy(tpe: Type, subtypes: List[KnownSubtype]): Tree = {
    val deps = subtypes.map({ case KnownSubtype(st, tree) => q"(${st.typeSymbol.name.toString}, $tree)" })
    q"$SealedHierarchyTCObj[$tpe](${tpe.toString}, List(..$deps))"
  }

  def forUnknown(tpe: Type): Tree =
    q"$UnknownTCObj[$tpe](${tpe.toString})"

  def assertSameTypes(expected: Type, actual: Type): Unit = {
    if (!(expected =:= actual)) {
      abort(s"Types don't match, expected $expected")
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

    val ApplyUnapply(_, unapply, params) = applyUnapplyFor(ttpe)
      .getOrElse(c.abort(c.enclosingPosition,
        s"Could not find unambiguous, matching pair of apply/unapply methods for $ttpe"))

    val companion = unapply.owner.asClass.module

    val expectedTpe = params match {
      case Nil => typeOf[Unit]
      case List((single, _)) => single.typeSignature
      case _ => getType(tq"(..${params.map(_._1.typeSignature)})")
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