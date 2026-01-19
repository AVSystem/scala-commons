package com.avsystem.commons
package analyzer

import scala.annotation.tailrec
import scala.tools.nsc.Global

class BadSingletonComponent(g: Global) extends AnalyzerRule(g, "badSingletonComponent") {

  import global._

  lazy val componentsTpe: Type = classType("com.avsystem.commons.di.Components")
  lazy val cachedSym: Symbol = componentsTpe.member(TermName("cached"))

  object UnwrapApply {
    @tailrec def unapply(tree: Tree): Some[Tree] = tree match {
      case Apply(fun, _) => unapply(fun)
      case TypeApply(fun, _) => unapply(fun)
      case t => Some(t)
    }
  }

  object Unwrap {
    @tailrec def unapply(t: Tree): Some[Tree] = t match {
      case Block(Nil, expr) => unapply(expr)
      case Typed(expr, _) => unapply(expr)
      case Annotated(_, expr) => unapply(expr)
      case UnwrapApply(Select(prefix, _)) if prefix.tpe =:= t.tpe => unapply(prefix)
      case _ => Some(t)
    }
  }

  def analyze(unit: CompilationUnit): Unit =
    if (componentsTpe != NoType) {
      object traverser extends Traverser {
        override def traverse(tree: Tree): Unit = tree match {
          case mdef @ DefDef(_, _, Nil, Nil, _, Unwrap(app @ Apply(_, List(arg, _))))
              if app.symbol == cachedSym && mdef.symbol.owner.isClass &&
                ThisType(mdef.symbol.owner) <:< componentsTpe =>
            traverse(arg)
          case t if t.symbol == cachedSym =>
            report(
              t.pos,
              "singleton(...) macro can only be used as a body of a parameterless method in a Components trait implementation",
            )
          case _ =>
            super.traverse(tree)
        }
      }

      traverser.traverse(unit.body)
    }
}
