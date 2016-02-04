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
    * @param sym          symbol of the `apply` method parameter
    * @param defaultValue tree that evaluates to default value of the `apply` parameter or [[EmptyTree]]
    * @param instance     tree that evaluates to type class instance for type of this parameter
    */
  case class ApplyParam(sym: Symbol, defaultValue: Tree, instance: Tree)

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
    * @param tpe       the record type
    * @param companion symbol of the companion object of record type; can be used to refer to `apply` and
    *                  `unapply` methods
    * @param params    metadata for parameters of `apply` method
    */
  def forApplyUnapply(tpe: Type, companion: Symbol, params: List[ApplyParam]): Tree

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
    * Usually, you want to throw a [[TypecheckException]] to indicate that type class instance cannot be derived
    * for this type. You can use [[typecheckException]] method for this.
    */
  def forUnknown(tpe: Type): Tree

  def typeClassInstance(tpe: Type): Type = getType(tq"$typeClass[$tpe]")

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

  def autoDerive[T: c.WeakTypeTag]: Tree = abortOnTypecheckException {
    val tpe = weakTypeOf[T]
    val tcTpe = typeClassInstance(tpe)

    c.enclosingMacros.tail
      .map(ctx => internal.createImporter(ctx.universe).importTree(ctx.macroApplication))
      .collectFirst {
        // avoid going into infinite recursion during auto derivation
        case tree if tree.symbol == c.macroApplication.symbol && tree.tpe =:= tcTpe =>
          // will be replaced by reference to deferred instance
          q"$RecursiveImplicitMarkerObj.mark[$tcTpe]"
      }
      .getOrElse(abortOnTypecheckException(autoDeriveFor(weakTypeOf[T])))
  }

  def autoDeriveRecursively[T: c.WeakTypeTag](allow: Tree): Tree =
    autoDerive[T]

  def autoDeriveWrapped[T: c.WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    q"""
       implicit val ${c.freshName(TermName("allow"))}: $AutoDeriveRecursivelyCls[$typeClass] = null
       ${wrapInAuto(q"implicitly[${typeClassInstance(tpe)}]")}
     """
  }
}
