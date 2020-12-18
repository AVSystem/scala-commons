package com.avsystem.commons
package macros.di

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.collection.mutable.ListBuffer
import scala.reflect.macros.blackbox

class ComponentMacros(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) {

  import c.universe._

  def DiPkg = q"$CommonsPkg.di"
  def ComponentCls: Tree = tq"$DiPkg.Component"
  def ComponentObj: Tree = q"$DiPkg.Component"

  lazy val ComponentTpe: Type = getType(tq"$ComponentCls[_]")
  lazy val ComponentRefSym: Symbol = ComponentTpe.member(TermName("ref"))
  lazy val InjectSym: Symbol = getType(tq"$DiPkg.Components").member(TermName("inject"))

  object ComponentRef {
    def unapply(tree: Tree): Option[Tree] = tree match {
      case Select(component, TermName("ref")) if tree.symbol == ComponentRefSym =>
        Some(component)
      case Apply(conversion, List(component)) if conversion.symbol == InjectSym =>
        Some(component)
      case _ => None
    }
  }

  private def mkComponent(tpe: Type, sourceInfo: Tree, definition: Tree, dependsOn: Seq[Tree], flatten: Boolean, singleton: Boolean): Tree = {
    val depArrayName = c.freshName(TermName("deps"))
    val depsBuf = new ListBuffer[Tree]

    def prepareDependency(tree: Tree, depTpe: Type): Tree = {
      val dependencyTree = mkComponent(depTpe, sourceInfo, tree, Nil, flatten = true, singleton = false)
      val innerSymbols = dependencyTree.collect({ case t@(_: DefTree | _: Function | _: Bind) if t.symbol != null => t.symbol }).toSet
      dependencyTree.foreach { t =>
        if (t.symbol != null && !innerSymbols.contains(t.symbol) && posIncludes(definition.pos, t.symbol.pos)) {
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
        case _ =>
          super.transform(tree)
      }
    }

    val transformedDefinition = DependencyExtractor.transform(definition)
    depsBuf ++= dependsOn

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

      val result =
        q"""
         new $DiPkg.ComponentImpl[$tpe](
           $sourceInfo,
           $ScalaPkg.IndexedSeq(..${depsBuf.result()}),
           ($depArrayName: $ScalaPkg.IndexedSeq[$ScalaPkg.Any]) =>
             $ComponentObj.CreateResult.$resultCase($finalDefinition)
         )
         """

      if (singleton)
        q"${c.prefix}.cached($result)($sourceInfo)"
      else
        result
    }
  }

  private def ensureRangePositions(): Unit =
    if (!c.compilerSettings.contains("-Yrangepos")) {
      abort("Component related macros require -Yrangepos")
    }

  def prototype[T: c.WeakTypeTag](definition: Tree, dependsOn: Tree*)(sourceInfo: Tree): Tree = {
    ensureRangePositions()
    mkComponent(weakTypeOf[T], sourceInfo, definition, dependsOn, flatten = false, singleton = false)
  }

  def singleton[T: c.WeakTypeTag](definition: Tree, dependsOn: Tree*)(sourceInfo: Tree): Tree = {
    ensureRangePositions()
    mkComponent(weakTypeOf[T], sourceInfo, definition, dependsOn, flatten = false, singleton = true)
  }
}
