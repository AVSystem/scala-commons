package com.avsystem.commons
package macros

import scala.reflect.macros.blackbox

private[commons]
class TestMacros(val c: blackbox.Context) extends TypeClassDerivation {

  import c.universe._

  val TestObj = q"$CommonsPkg.macros.TypeClassDerivationTest"
  val SingletonTCObj = q"$TestObj.SingletonTC"
  val ApplyUnapplyTCObj = q"$TestObj.ApplyUnapplyTC"
  val SealedHierarchyTCObj = q"$TestObj.SealedHierarchyTC"
  val UnknownTCObj = q"$TestObj.UnknownTC"
  val DefValObj = q"$TestObj.DefVal"

  def typeClassInstance(tpe: Type): Type = getType(tq"$TestObj.TC[$tpe]")
  def implementDeferredInstance(tpe: Type): Tree = q"new $TestObj.TC.Deferred[$tpe]"

  def forSingleton(tpe: Type, singleValueTree: Tree): Tree =
    q"$SingletonTCObj[$tpe](${tpe.toString}, $singleValueTree)"

  def forApplyUnapply(au: ApplyUnapply, params: List[ApplyParam]): Tree = {
    val deps = params.map { case ApplyParam(_, s, dv, t) =>
      val defaultValueOpt = if (dv == EmptyTree) q"$NoneObj" else q"$SomeObj($DefValObj($dv))"
      q"(${s.name.toString}, $t, $defaultValueOpt)"
    }
    q"$ApplyUnapplyTCObj[${au.ownerTpe}](${au.ownerTpe.toString}, $ListObj(..$deps))"
  }

  def forSealedHierarchy(tpe: Type, subtypes: List[KnownSubtype]): Tree = {
    val deps = subtypes.map({ case KnownSubtype(_, st, tree) => q"(${st.typeSymbol.name.toString}, $tree)" })
    q"$SealedHierarchyTCObj[$tpe](${tpe.toString}, $ListObj(..$deps))"
  }

  def forUnknown(tpe: Type): Tree =
    q"$UnknownTCObj[$tpe](${tpe.toString})"

  def assertSameTypes(expected: Type, actual: Type): Unit = {
    if (!(expected =:= actual)) {
      abort(s"Types don't match, expected $expected")
    }
  }

  def testTreeForType(tpeRepr: Tree): Tree = {
    val Literal(Constant(repr)) = tpeRepr

    val Typed(_, tpt) = c.parse(s"(??? : $repr)")
    val tpe = getType(tpt)
    val newTree = treeForType(tpe)
    val newTpe = getType(newTree)

    assertSameTypes(tpe, newTpe)
    q"$PredefObj.???"
  }

  def testKnownSubtypes[T: WeakTypeTag, R: WeakTypeTag]: Tree = instrument {
    val expectedResultTpe = knownSubtypes(weakTypeOf[T])
      .map(types => getType(tq"(..$types)"))
      .getOrElse(typeOf[Nothing])

    assertSameTypes(expectedResultTpe, weakTypeOf[R])
    q"$PredefObj.???"
  }

  val ApplierUnapplierCls = tq"$CommonsPkg.macros.ApplierUnapplier"

  def applierUnapplier[T: WeakTypeTag, F: WeakTypeTag]: Tree = instrument {
    val ttpe = weakTypeOf[T]
    val ftpe = weakTypeOf[F]

    val au = applyUnapplyFor(ttpe)
      .getOrElse(c.abort(c.enclosingPosition,
        s"Could not find unambiguous, matching pair of apply/unapply methods for $ttpe"))

    val companion = au.unapply.owner.asClass.module

    val expectedTpe = au.params match {
      case Nil => typeOf[Unit]
      case List(single) => single.typeSignature
      case _ => getType(tq"(..${au.params.map(_.typeSignature)})")
    }
    assertSameTypes(expectedTpe, ftpe)

    val applyParams = au.params match {
      case List(_) => List(Ident(TermName("f")))
      case _ => au.params.indices.map(i => q"f.${TermName(s"_${i + 1}")}")
    }

    val unapplyResult = au.params match {
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