package com.avsystem.commons
package macros

import scala.reflect.macros.blackbox

trait TypeClassDerivation extends MacroCommons {

  import c.universe._

  final def DeferredInstanceCls: Tree = tq"$CommonsPkg.derivation.DeferredInstance"
  final def AllowImplicitMacroCls: Tree = tq"$CommonsPkg.derivation.AllowImplicitMacro"
  final def AllowImplicitMacroObj: Tree = q"$CommonsPkg.derivation.AllowImplicitMacro"
  final def RecursiveImplicitMarkerObj: Tree = q"$CommonsPkg.macros.RecursiveImplicitMarker"

  /**
    * Returns tree that instantiates a "deferred instance" of this type class. Deferred instance
    * is a special implementation of the type class which implements the `com.avsystem.commons.derivation.DeferredInstance`
    * trait and wraps an another, actual instance of the type class and delegates all operations to that
    * wrapped instance. The wrapped instance itself is supplied later, by assigning a var available on the deferred
    * instance.
    *
    * This is all necessary to handle automatic derivation for recursively-defined types like:
    * {{{
    *   case class Tree(children: List[Tree])
    * }}}
    *
    * EXAMPLE:
    *
    * Let's assume a type class `Traverser` defined like this:
    * {{{
    *   trait Traverser[T] {
    *     def traverse(value: T): Unit
    *   }
    *   object Traverser {
    *     class Deferred[T] extends DeferredInstance[Traverser[T]] with Traverser[T] {
    *       def traverse(value: T) = underlying.traverse(value)
    *     }
    *
    *     implicit def forList[T](implicit forElement: Traverser[T]): Traverser[List[T]] =
    *       new Traverser[List[T]] {
    *         def traverse(value: List[T]) = value.foreach(forElement.traverse)
    *       }
    *   }
    * }}}
    *
    * Automatically derived type class instance for `Tree` would then look somewhat like this:
    *
    * {{{
    *   val tcTree: Traverser[Tree] = {
    *     val deferred: DeferredInstance[Traverser[Tree]] with Traverser[Tree] = new Traverser.Deferred[T]
    *     deferred.underlying = new Traverser[Tree] {
    *       val forChildren = Traverser.forList[Tree](deferred)
    *       def traverse(value: Tree) = value.children.foreach(forChildren.traverse)
    *     }
    *     deferred.underlying
    *   }
    * }}}
    */
  def implementDeferredInstance(tpe: Type): Tree

  /**
    * Contains metadata extracted from `apply` method of companion object of some record (case-class like) type.
    *
    * @param sym          symbol of the `apply` method parameter or case class constructor parameter
    *                     (if `apply` is auto-generated for case class companion object)
    * @param defaultValue tree that evaluates to default value of the `apply` parameter or `EmptyTree`
    * @param instance     tree that evaluates to type class instance for type of this parameter
    */
  case class ApplyParam(idx: Int, sym: TermSymbol, defaultValue: Tree, instance: Tree) {
    val repeated: Boolean = isRepeated(sym)
    def valueType: Type = actualParamType(sym)
    def asArgument(tree: Tree): Tree = if (repeated) q"$tree: _*" else tree
  }

  /**
    * Contains metadata extracted from one of the case subtypes in a sealed hierarchy.
    *
    * @param tpe      the case subtype itself
    * @param instance tree that evaluates to type class instance for this subtype
    */
  case class KnownSubtype(idx: Int, tpe: Type, instance: Tree) {
    def sym: Symbol = tpe.typeSymbol
  }

  /**
    * Derives type class instance for singleton type (i.e. an `object` or `this`)
    *
    * @param tpe             the singleton type
    * @param singleValueTree a tree that evaluates to the sole value of the singleton type
    */
  def forSingleton(tpe: Type, singleValueTree: Tree): Tree

  /**
    * Derives type class instance for record type. Record type is a class/trait whose companion object has
    * matching `apply` and `unapply` methods. In particular, every case class is a proper record type.
    *
    * @param applyUnapply info about case class or case class like type
    * @param params       metadata for parameters of `apply` method
    */
  def forApplyUnapply(applyUnapply: ApplyUnapply, params: List[ApplyParam]): Tree

  /**
    * Derives type class instance for union type (sealed hierarchy in which every non-abstract subtype has
    * the type class instance of its own or can also be automatically derived).
    *
    * @param tpe      type of the sealed class/trait
    * @param subtypes metadata for all direct non-abstract subtypes of this sealed class/trait
    */
  def forSealedHierarchy(tpe: Type, subtypes: List[KnownSubtype]): Tree

  /**
    * Derives type class instance for arbitrary type which is neither a singleton, record nor union type.
    * Usually, you want to throw a `TypecheckException` to indicate that type class instance cannot be derived
    * for this type. You can use [[typecheckException]] method for this.
    */
  def forUnknown(tpe: Type): Tree

  def typeClassInstance(tpe: Type): Type
  def dependencyType(tpe: Type): Type = typeClassInstance(tpe)

  def dependency(depTpe: Type, tcTpe: Type, param: Symbol): Tree = {
    val clue = s"Cannot materialize $tcTpe because of problem with parameter ${param.name}: "
    val depTcTpe = dependencyType(depTpe)
    q"""$ImplicitsObj.infer[$depTcTpe](${internal.setPos(StringLiteral(clue), param.pos)})"""
  }

  def materializeFor(tpe: Type): Tree = {
    val dtpe = tpe.dealias
    val tcTpe = typeClassInstance(dtpe)

    def singleTypeTc: Option[Tree] =
      singleValueFor(dtpe).map(tree => forSingleton(dtpe, tree))

    def applyUnapplyTc: Option[Tree] = applyUnapplyFor(dtpe).map { au =>
      val dependencies = au.params.zipWithIndex.map { case (s, idx) =>
        val defaultValue = au.defaultValueFor(s, idx)
        ApplyParam(idx, s, defaultValue, dependency(actualParamType(s), tcTpe, s))
      }
      forApplyUnapply(au, dependencies)
    }

    def sealedHierarchyTc: Option[Tree] = knownSubtypes(dtpe).map { subtypes =>
      if (subtypes.isEmpty) {
        abort(s"Could not find any subtypes for $dtpe")
      }
      val dependencies = subtypes.zipWithIndex.map { case (depTpe, idx) =>
        val depTree = c.inferImplicitValue(dependencyType(depTpe), withMacrosDisabled = true) match {
          case EmptyTree => q"${c.prefix}.materialize[$depTpe]"
          case t => t
        }
        KnownSubtype(idx, depTpe, depTree)
      }
      forSealedHierarchy(dtpe, dependencies)
    }

    singleTypeTc orElse applyUnapplyTc orElse sealedHierarchyTc getOrElse forUnknown(dtpe)
  }

  def withRecursiveImplicitGuard(targetTpe: Type, unguarded: Tree): Tree = {
    val dtpe = targetTpe.dealias
    val tcTpe = typeClassInstance(dtpe)

    // deferred instance is necessary to handle recursively defined types
    val deferredName = c.freshName(TermName("deferred"))
    // introducing intermediate val to make sure exact type of materialized instance is not lost
    val underlyingName = c.freshName(TermName("underlying"))
    registerImplicit(tcTpe, deferredName)

    val withDummyImplicit =
      q"""
        implicit def $deferredName: $DeferredInstanceCls[$tcTpe] with $tcTpe = $PredefObj.???
        $unguarded
       """

    def guarded: Tree =
      q"""
        implicit val $deferredName: $DeferredInstanceCls[$tcTpe] with $tcTpe =
          ${implementDeferredInstance(dtpe)}
        val $underlyingName = $unguarded
        $deferredName.underlying = $underlyingName
        $underlyingName
       """

    c.typecheck(withDummyImplicit, silent = true) match {
      case Block(List(deferredDef), typedUnguarded)
        if !typedUnguarded.exists(_.symbol == deferredDef.symbol) => typedUnguarded
      case _ => guarded
    }
  }

  def materialize[T: WeakTypeTag]: Tree = instrument {
    val tpe = weakTypeOf[T]
    withRecursiveImplicitGuard(tpe, materializeFor(tpe))
  }

  def materializeImplicitly[T: WeakTypeTag](allow: Tree): Tree =
    instrument(materialize[T])
}

abstract class AbstractTypeClassDerivation(c: blackbox.Context)
  extends AbstractMacroCommons(c) with TypeClassDerivation
