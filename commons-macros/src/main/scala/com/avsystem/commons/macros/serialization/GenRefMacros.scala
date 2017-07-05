package com.avsystem.commons
package macros.serialization

import scala.reflect.macros.blackbox

class GenRefMacros(ctx: blackbox.Context) extends CodecMacroCommons(ctx) {

  import c.universe._

  def rawRef[S: c.WeakTypeTag, T: c.WeakTypeTag](fun: Tree): Tree = fun match {
    case Function(List(param), body) =>
      var refs = List.empty[Tree]
      def extract(body: Tree): Unit = body match {
        case Ident(_) if body.symbol == param.symbol =>
        case Apply(prefix, Nil) => extract(prefix)
        case Typed(prefix, _) => extract(prefix)
        case Select(prefix, _) =>
          if (body.symbol.isTerm && body.symbol.asTerm.isCaseAccessor) {
            val constrArg = alternatives(prefix.tpe.member(termNames.CONSTRUCTOR))
              .find(_.asMethod.isPrimaryConstructor)
              .getOrElse(abort(s"No primary constructor found for ${prefix.tpe}"))
              .typeSignature.paramLists.head.find(_.name == body.symbol.name)
              .getOrElse(abort(s"No primary constructor parameter ${body.symbol.name} found"))
            refs ::= q"$SerializationPkg.RawRef.Field(${annotName(constrArg)})"
            extract(prefix)
          } else {
            c.abort(body.pos, s"${body.symbol} is not a case class field accessor")
          }
        case _ =>
          c.abort(body.pos, "This invocation can't be translated into RawRef")
      }
      extract(body)
      refs match {
        case Nil => q"$SerializationPkg.RawRef.Identity"
        case List(tree) => tree
        case _ => refs.reduce((t1, t2) => q"$SerializationPkg.RawRef.Composite($t1, $t2)")
      }
    case _ => c.abort(fun.pos, s"Expected lambda expression")
  }

  def genRef[S: c.WeakTypeTag, T: c.WeakTypeTag](fun: Tree): Tree =
    q"$SerializationPkg.GenRef($fun, ${rawRef[S, T](fun)})"
}
