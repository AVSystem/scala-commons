package com.avsystem.commons
package macros

import scala.reflect.macros.TypecheckException

/**
  * Author: ghik
  * Created: 04/12/15.
  */
trait TypeClassDerivation extends MacroCommons {

  import c.universe._

  val DeferredInstanceCls = tq"$CommonsPackage.derivation.DeferredInstance"
  val AutoDeriveRecursivelyCls = tq"$CommonsPackage.derivation.AutoDeriveRecursively"
  val RecursiveImplicitMarkerObj = q"$CommonsPackage.macros.RecursiveImplicitMarker"

  def typeClass: Tree
  def typeClassName: String
  def wrapInAuto(tree: Tree): Tree
  def implementDeferredInstance(tpe: Type): Tree

  def typeClassInstance(tpe: Type): Type = getType(tq"$typeClass[$tpe]")

  case class ApplyParam(sym: Symbol, defaultValue: Tree, instance: Tree)
  case class KnownSubtype(tpe: Type, instance: Tree) {
    def sym = tpe.typeSymbol
  }

  def forSingleton(tpe: Type, singleValueTree: Tree): Tree
  def forApplyUnapply(tpe: Type, companion: Symbol, params: List[ApplyParam]): Tree
  def forSealedHierarchy(tpe: Type, subtypes: List[KnownSubtype]): Tree
  def forUnknown(tpe: Type): Tree

  def autoDeriveFor(tpe: Type): Tree = {
    val tcTpe = typeClassInstance(tpe)

    def handleMissingDependency[T](hint: String)(expr: => T): T =
      try expr catch {
        case TypecheckException(pos, msg) =>
          throw new TypecheckException(pos, s"Cannot automatically derive $typeClassName for $tpe because ($hint):\n$msg")
      }

    def inferDependency(depTpe: Type, silent: Boolean = false): Tree =
      c.typecheck(q"implicitly[${typeClassInstance(depTpe)}]", silent = silent) match {
        case Apply(_, List(arg)) => arg
        case EmptyTree => EmptyTree
      }

    def singleTypeTc = singleValueFor(tpe).map(tree => forSingleton(tpe, tree))
    def applyUnapplyTc = applyUnapplyFor(tpe).map {
      case ApplyUnapply(companion, params) =>
        val dependencies = params.map { case (s, defaultValue) =>
          ApplyParam(s, defaultValue, handleMissingDependency(s"for field ${s.name}")(inferDependency(s.typeSignature)))
        }
        forApplyUnapply(tpe, companion, dependencies)
    }
    def sealedHierarchyTc = knownSubtypes(tpe).map { subtypes =>
      val dependencies = subtypes.map { depTpe =>
        val depTree = handleMissingDependency(s"for case type $depTpe") {
          inferDependency(depTpe, silent = true) match {
            case EmptyTree => autoDeriveFor(depTpe)
            case tree => tree
          }
        }
        KnownSubtype(depTpe, depTree)
      }
      withKnownSubclassesCheck(forSealedHierarchy(tpe, dependencies), tpe)
    }
    val result = singleTypeTc orElse applyUnapplyTc orElse sealedHierarchyTc getOrElse forUnknown(tpe)

    val deferredName = c.freshName(TermName("deferred"))
    lazy val Block(List(deferredVal), deferredIdent) =
      c.typecheck(q"val $deferredName: $DeferredInstanceCls[$tcTpe] with $tcTpe = ${implementDeferredInstance(tpe)}; $deferredName")

    // the transformer replaces recursive implicits with references to 'deferred instance'
    object transformer extends Transformer {
      var changed = false
      override def transform(tree: Tree): Tree = {
        if (tree.symbol != null && tree.tpe != null) {
          if (!tree.isDef && tree.symbol.isImplicit && tree.tpe.widen =:= tcTpe) {
            changed = true
            deferredIdent.duplicate
          } else super.transform(tree)
        } else super.transform(tree)
      }
    }
    Some(transformer.transform(result)).filter(_ => transformer.changed).map { guardedResult =>
      q"""
         $deferredVal
         ${deferredIdent.duplicate}.underlying = $guardedResult
         ${deferredIdent.duplicate}.underlying
         """
    }.getOrElse(result)
  }

  def autoDerive[T: c.WeakTypeTag]: Tree =
    abortOnTypecheckException(autoDeriveFor(weakTypeOf[T]))

  def autoDeriveRecursively[T: c.WeakTypeTag](allow: Tree): Tree = {
    val tpe = weakTypeOf[T]
    val tcTpe = typeClassInstance(tpe)

    c.enclosingMacros.tail
      .map(ctx => internal.createImporter(ctx.universe).importTree(ctx.macroApplication))
      .collectFirst {
        // avoid going into infinite recursion during derivation
        case tree if tree.symbol == c.macroApplication.symbol && tree.tpe =:= tcTpe =>
          // will be replaced by reference to 'deferred instance'
          q"$RecursiveImplicitMarkerObj.mark[$tcTpe]"
      }
      .getOrElse(abortOnTypecheckException(autoDeriveFor(weakTypeOf[T])))
  }

  def autoDeriveWrapped[T: c.WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    q"""
       implicit val ${c.freshName(TermName("allow"))}: $AutoDeriveRecursivelyCls[$typeClass] = null
       ${wrapInAuto(q"implicitly[${typeClassInstance(tpe)}]")}
     """
  }
}
