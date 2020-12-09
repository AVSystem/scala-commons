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

  def cachedComponentCreate[T: c.WeakTypeTag](definition: Tree): Tree =
    q"${c.prefix}.cacheComponent(${c.prefix}.component($definition))"

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
      val innerSymbols = tree.collect({ case t@(_: DefTree | _: Function | _: Bind) if t.symbol != null => t.symbol }).toSet
      tree.foreach { t =>
        if (t.symbol != null && !innerSymbols.contains(t.symbol) && ownersOf(t.symbol).contains(enclosingSym)) {
          errorAt(s"you cannot use local values or methods in an expression with .ref called on it", t.pos)
        }
        t match {
          case ComponentRef(_) =>
            errorAt("invalid nested component reference - " +
              "you cannot use .ref inside an expression that itself has .ref called on it", t.pos)
          case _ =>
        }
      }
      if (innerSymbols.nonEmpty) c.untypecheck(tree) else tree
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
    val needsRetyping = transformedDefinition != definition ||
      definition.exists {
        case _: DefTree | _: Function | _: Bind => true
        case _ => false
      }

    q"""
       new $ComponentCls[$ttpe] {
         def name: $PredefObj.String = $name
         val sourceInfo: $MiscPkg.SourceInfo = $ImplicitsObj.infer[$MiscPkg.SourceInfo]

         lazy val dependencies: $ScalaPkg.IndexedSeq[$ComponentTpe] =
           $ScalaPkg.IndexedSeq(..${depsBuf.result()})

         protected def create($depArrayName: $ScalaPkg.IndexedSeq[$ScalaPkg.Any]): $ttpe =
           ${if (needsRetyping) c.untypecheck(transformedDefinition) else definition}
       }
       """
  }
}
