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

  private def mkComponent(tpe: Type, sourceInfo: Tree, definition: Tree, singleton: Boolean): Tree = {
    val depArrayName = c.freshName(TermName("deps"))
    val depsBuf = new ListBuffer[Tree]

    def validateDependency(tree: Tree): Tree = {
      val innerSymbols = tree.collect({ case t@(_: DefTree | _: Function | _: Bind) if t.symbol != null => t.symbol }).toSet
      val needsRetyping = innerSymbols.nonEmpty || tree.exists {
        case _: DefTree | _: Function | _: Bind => true
        case _ => false
      }
      tree.foreach {
        case t@ComponentRef(_) =>
          errorAt(s"illegal nested component reference inside expression representing component dependency", t.pos)
        case t if t.symbol != null && !innerSymbols.contains(t.symbol) && posIncludes(definition.pos, t.symbol.pos) =>
          errorAt(s"illegal local value or method reference inside expression representing component dependency", t.pos)
        case _ =>
      }
      if (needsRetyping) c.untypecheck(tree) else tree
    }

    object DependencyExtractor extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case ComponentRef(component) =>
          depsBuf += validateDependency(component)
          val depTpe = component.tpe.baseType(ComponentTpe.typeSymbol).typeArgs.head
          q"$depArrayName(${depsBuf.size - 1}).asInstanceOf[$depTpe]"
        case _ =>
          super.transform(tree)
      }
    }

    val transformedDefinition = DependencyExtractor.transform(definition)

    val needsRetyping = transformedDefinition != definition ||
      definition.exists {
        case _: DefTree | _: Function | _: Bind => true
        case _ => false
      }
    val finalDefinition =
      if (needsRetyping) c.untypecheck(transformedDefinition) else definition

    val result =
      q"""
        new $DiPkg.Component[$tpe](
          ${c.prefix}.componentInfo($sourceInfo),
          $ScalaPkg.IndexedSeq(..${depsBuf.result()}),
          ($depArrayName: $ScalaPkg.IndexedSeq[$ScalaPkg.Any]) => $finalDefinition
        )
       """

    if (singleton)
      q"${c.prefix}.cached($result, ${c.prefix}.componentInfo($sourceInfo))"
    else
      result
  }

  private def ensureRangePositions(): Unit =
    if (!c.compilerSettings.contains("-Yrangepos")) {
      abort("Component related macros require -Yrangepos")
    }

  def prototype[T: c.WeakTypeTag](definition: Tree)(sourceInfo: Tree): Tree = {
    ensureRangePositions()
    mkComponent(weakTypeOf[T], sourceInfo, definition, singleton = false)
  }

  def singleton[T: c.WeakTypeTag](definition: Tree)(sourceInfo: Tree): Tree = {
    ensureRangePositions()
    mkComponent(weakTypeOf[T], sourceInfo, definition, singleton = true)
  }

  def autoComponent[T: c.WeakTypeTag](definition: Tree)(sourceInfo: Tree): Tree = {
    ensureRangePositions()
    val component = mkComponent(weakTypeOf[T], sourceInfo, definition, singleton = false)
    q"$DiPkg.AutoComponent($component)"
  }

  def reifyAllSingletons: Tree = {
    val prefixName = c.freshName(TermName("prefix"))
    val bufName = c.freshName(TermName("buf"))

    val componentMethods =
      c.prefix.actualType.members.iterator
        .filter(s => s.isMethod && !s.isSynthetic).map(_.asMethod)
        .filter { m =>
          m.typeParams.isEmpty && m.paramLists.isEmpty &&
            m.typeSignatureIn(c.prefix.actualType).resultType <:< ComponentTpe
        }
        .toList

    q"""
       val $prefixName = ${c.prefix}
       val $bufName = new $CollectionPkg.mutable.ListBuffer[$ComponentTpe]
       def addIfCached(_c: $ComponentTpe): Unit =
         if(_c.isCached) $bufName += _c
       ..${componentMethods.map(m => q"addIfCached($prefixName.$m)")}
       $bufName.result()
       """
  }
}
