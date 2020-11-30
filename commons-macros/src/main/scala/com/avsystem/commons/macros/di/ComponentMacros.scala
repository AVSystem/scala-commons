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

  def componentCreate[T: c.WeakTypeTag](definition: Tree): Tree = {
    val enclosingSym = {
      val sym = c.internal.enclosingOwner
      if (!sym.isTerm || !(sym.asTerm.isVal || sym.asTerm.isMethod)) {
        abort("component(...) must be assigned to a val or def")
      }
      sym.asTerm.getter
    }

    val name = enclosingSym.name.decodedName.toString

    val ttpe = weakTypeOf[T]
    val depArrayName = c.freshName(TermName("deps"))
    val depsBuf = new ListBuffer[Tree]

    def validateDep(tree: Tree): Tree = {
      val innerSymbols = tree.collect({ case defTree: DefTree if defTree.symbol != null => defTree.symbol }).toSet
      tree.foreach { t =>
        if (t.symbol != null && !innerSymbols.contains(t.symbol) && ownersOf(t.symbol).contains(enclosingSym)) {
          errorAt(s"this symbol cannot be used here because of usage of .ref", t.pos)
        }
      }
      if(innerSymbols.nonEmpty) c.untypecheck(tree) else tree
    }

    object DependencyExtractor extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case ComponentRef(component) =>
          depsBuf += validateDep(component)
          val depTpe = component.tpe.baseType(ComponentTpe.typeSymbol).typeArgs.head
          q"$depArrayName(${depsBuf.size - 1}).asInstanceOf[$depTpe]"
        case Select(_, TermName("componentName")) if tree.symbol == ComponentNameSym =>
          q"$ComponentNameObj($name)"
        case _ =>
          super.transform(tree)
      }
    }

    val transformedDefinition = DependencyExtractor.transform(definition)

    q"""
       new $ComponentCls[$ttpe] {
         def name: $PredefObj.String = $name

         lazy val dependencies: $CollectionPkg.IndexedSeq[$ComponentTpe] =
           $ScalaPkg.IndexedSeq(..${depsBuf.result()})

         protected def create($depArrayName: $ScalaPkg.IndexedSeq[$ScalaPkg.Any]): $ttpe =
           ${c.untypecheck(transformedDefinition)}
       }
       """
  }
}
