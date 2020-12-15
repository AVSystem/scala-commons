package com.avsystem.commons
package macros.di

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.collection.mutable.ListBuffer
import scala.reflect.macros.blackbox

class ComponentMacros(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) {

  import c.universe._

  def DiPkg = q"$CommonsPkg.di"
  def ComponentNameObj: Tree = q"$DiPkg.ComponentName"
  def ComponentCls: Tree = tq"$DiPkg.Component"
  def ComponentObj: Tree = q"$DiPkg.Component"

  lazy val ComponentTpe: Type = getType(tq"$ComponentCls[_]")
  lazy val ComponentRefSym: Symbol = ComponentTpe.member(TermName("ref"))
  lazy val InjectSym: Symbol = getType(tq"$DiPkg.Components").member(TermName("inject"))
  lazy val ComponentNameSym: Symbol = getType(tq"$DiPkg.ComponentName.type").member(TermName("componentName"))

  object ComponentRef {
    def unapply(tree: Tree): Option[Tree] = tree match {
      case Select(component, TermName("ref")) if tree.symbol == ComponentRefSym =>
        Some(component)
      case Apply(conversion, List(component)) if conversion.symbol == InjectSym =>
        Some(component)
      case _ => None
    }
  }

  def cachedComponentCreate[T: c.WeakTypeTag](definition: Tree): Tree =
    q"${c.prefix}.cached(${c.prefix}.component($definition))"

  private def mkComponent(tpe: Type, definition: Tree, flatten: Boolean): Tree = {
    val enclosingSym = {
      val sym = c.internal.enclosingOwner
      if (!sym.isTerm || !(sym.asTerm.isVal || sym.asTerm.isMethod)) {
        abort("component(...) must be assigned to a val or def")
      }
      sym.asTerm.getter
    }

    val name = enclosingSym.name.decodedName.toString

    val depArrayName = c.freshName(TermName("deps"))
    val depsBuf = new ListBuffer[Tree]

    def prepareDependency(tree: Tree, depTpe: Type): Tree = {
      val dependencyTree = mkComponent(depTpe, tree, flatten = true)
      val innerSymbols = dependencyTree.collect({ case t@(_: DefTree | _: Function | _: Bind) if t.symbol != null => t.symbol }).toSet
      dependencyTree.foreach { t =>
        if (t.symbol != null && !innerSymbols.contains(t.symbol) && ownersOf(t.symbol).contains(enclosingSym)) {
          errorAt(s"due to parallel initialization you cannot use local values or methods in an expression representing component dependency", t.pos)
        }
      }
      if (innerSymbols.nonEmpty) c.untypecheck(dependencyTree) else dependencyTree
    }

    object DependencyExtractor extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case ComponentRef(component) =>
          depsBuf += prepareDependency(component, tree.tpe)
          val depTpe = component.tpe.baseType(ComponentTpe.typeSymbol).typeArgs.head
          q"$depArrayName(${depsBuf.size - 1}).asInstanceOf[$depTpe]"
        case Select(_, TermName("componentName")) if tree.symbol == ComponentNameSym =>
          q"$ComponentNameObj($name)"
        case _ =>
          super.transform(tree)
      }
    }

    val transformedDefinition = DependencyExtractor.transform(definition)

    if (flatten && depsBuf.isEmpty) definition
    else {
      val needsRetyping = transformedDefinition != definition ||
        definition.exists {
          case _: DefTree | _: Function | _: Bind => true
          case _ => false
        }
      val finalDefinition =
        if (needsRetyping) c.untypecheck(transformedDefinition) else definition

      val resultCase = TermName(if (flatten) "More" else "Ready")

      q"""
       new $DiPkg.ComponentImpl[$tpe](
         $name,
         $ImplicitsObj.infer[$MiscPkg.SourceInfo],
         $ScalaPkg.IndexedSeq(..${depsBuf.result()}),
         ($depArrayName: $ScalaPkg.IndexedSeq[$ScalaPkg.Any]) => $ComponentObj.CreateResult.$resultCase($finalDefinition)
       )
       """
    }
  }

  def componentCreate[T: c.WeakTypeTag](definition: Tree): Tree =
    mkComponent(weakTypeOf[T], definition, flatten = false)
}
