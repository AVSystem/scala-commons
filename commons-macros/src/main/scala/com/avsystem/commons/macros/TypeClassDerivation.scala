package com.avsystem.commons
package macros

import scala.reflect.macros.blackbox

trait TypeClassDerivation extends MacroCommons {

  import c.universe._

  val DeferredInstanceCls = tq"$CommonsPackage.derivation.DeferredInstance"
  val AllowImplicitMacroCls = tq"$CommonsPackage.derivation.AllowImplicitMacro"
  val AllowImplicitMacroObj = q"$CommonsPackage.derivation.AllowImplicitMacro"
  val RecursiveImplicitMarkerObj = q"$CommonsPackage.macros.RecursiveImplicitMarker"

  /**
    * A tree that represents type constructor of the type class to be derived.
    */
  def typeClass: Tree

  /**
    * Human-friendly name of the type class. Used in error messages.
    */
  def typeClassName: String

  /**
    * Wraps the tree that evaluates to some instance of the type class into a tree that evaluates to an
    * "auto" version of this type class.
    */
  def wrapInAuto(tree: Tree): Tree

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
  case class ApplyParam(sym: TermSymbol, defaultValue: Tree, instance: Tree)

  /**
    * Contains metadata extracted from one of the case subtypes in a sealed hierarchy.
    *
    * @param tpe      the case subtype itself
    * @param instance tree that evaluates to type class instance for this subtype
    */
  case class KnownSubtype(tpe: Type, instance: Tree) {
    def sym = tpe.typeSymbol
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
    * @param tpe     the record type
    * @param apply   symbol of the `apply` method in companion object
    * @param unapply symbol of the `unapply` method in companion object
    * @param params  metadata for parameters of `apply` method
    */
  def forApplyUnapply(tpe: Type, apply: Symbol, unapply: Symbol, params: List[ApplyParam]): Tree

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

  def typeClassInstance(tpe: Type): Type = getType(tq"$typeClass[$tpe]")

  def materializeFor(tpe: Type): Tree = {
    val tcTpe = typeClassInstance(tpe)

    def dependency(depTpe: Type, hint: String, allowImplicitMacro: Boolean = false): Tree = {
      val clue = s"Cannot automatically derive type class instance $tcTpe because ($hint):\n"
      val depTcTpe = typeClassInstance(depTpe)
      val allowDef = if (allowImplicitMacro)
        q"""
          implicit val ${c.freshName(TermName("allow"))}: $AllowImplicitMacroCls[$depTcTpe] =
            $AllowImplicitMacroObj[$depTcTpe]
         """
      else
        q"()"

      q"""
        $allowDef
        $ImplicitsObj.infer[$depTcTpe]($clue)
       """
    }

    def singleTypeTc = singleValueFor(tpe).map(tree => forSingleton(tpe, tree))
    def applyUnapplyTc = applyUnapplyFor(tpe).map {
      case ApplyUnapply(apply, unapply, params) =>
        val dependencies = params.map { case (s, defaultValue) =>
          ApplyParam(s, defaultValue, dependency(s.typeSignature, s"for field ${s.name}"))
        }
        forApplyUnapply(tpe, apply, unapply, dependencies)
    }
    def sealedHierarchyTc = knownSubtypes(tpe).map { subtypes =>
      if (subtypes.isEmpty) {
        abort(s"Could not find any subtypes for $tpe")
      }
      val dependencies = subtypes.map { depTpe =>
        val depTree = dependency(depTpe, s"for case type $depTpe", allowImplicitMacro = true)
        KnownSubtype(depTpe, depTree)
      }
      withKnownSubclassesCheck(forSealedHierarchy(tpe, dependencies), tpe)
    }

    val untypedResult = singleTypeTc orElse applyUnapplyTc orElse sealedHierarchyTc getOrElse forUnknown(tpe)

    val deferredName = c.freshName(TermName("deferred"))
    val guardedResult@Block(List(_, Apply(_, List(result))), _) = c.typecheck(
      q"""
        implicit val $deferredName: $DeferredInstanceCls[$tcTpe] with $tcTpe =
          ${implementDeferredInstance(tpe)}
        $deferredName.underlying = $untypedResult
        $deferredName.underlying
       """
    )

    val needsGuarding = result.exists {
      case Ident(`deferredName`) => true
      case _ => false
    }

    if (needsGuarding) guardedResult else result
  }

  def materialize[T: c.WeakTypeTag]: Tree =
    abortOnTypecheckException(materializeFor(weakTypeOf[T]))

  def materializeImplicitly[T: c.WeakTypeTag](allow: Tree): Tree =
    materialize[T]

  def materializeAuto[T: c.WeakTypeTag]: Tree = {
    val tcTpe = typeClassInstance(weakTypeOf[T])
    val tName = c.freshName(TypeName("T"))
    q"""
       implicit def ${c.freshName(TermName("allow"))}[$tName]: $AllowImplicitMacroCls[$typeClass[$tName]] =
         $AllowImplicitMacroObj[$typeClass[$tName]]
       ${wrapInAuto(q"""$ImplicitsObj.infer[$tcTpe]("")""")}
     """
  }
}

abstract class AbstractTypeClassDerivation(c: blackbox.Context)
  extends AbstractMacroCommons(c) with TypeClassDerivation