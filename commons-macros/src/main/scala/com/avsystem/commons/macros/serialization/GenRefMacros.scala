package com.avsystem.commons
package macros.serialization

import java.util

import scala.collection
import scala.reflect.macros.blackbox

class GenRefMacros(ctx: blackbox.Context) extends CodecMacroCommons(ctx) {

  import c.universe._

  val GenRefTpe = getType(tq"$SerializationPkg.GenRef[_,_]")
  val MapTpe = typeOf[collection.Map[_, _]]
  val JMapTpe = typeOf[util.Map[_, _]]
  val MapApply = MapTpe.member(TermName("apply"))
  val JMapGet = JMapTpe.member(TermName("get"))

  object MapApplyOrGet {
    def unapply(tree: Tree): Option[(Tree, Tree, Symbol)] = tree match {
      case Apply(Select(prefix, TermName("apply")), List(arg))
        if (tree.symbol :: tree.symbol.overrides).contains(MapApply) =>
        Some((prefix, arg, MapTpe.typeSymbol))
      case Apply(Select(prefix, TermName("get")), List(arg))
        if (tree.symbol :: tree.symbol.overrides).contains(JMapGet) =>
        Some((prefix, arg, JMapTpe.typeSymbol))
      case _ => None
    }
  }

  def rawRef(fun: Tree): Tree = fun match {
    case genRef if genRef.tpe <:< GenRefTpe => q"${genRef.duplicate}.rawRef"
    case Apply(TypeApply(Select(left, TermName("andThen")), List(_)), List(right)) =>
      q"$SerializationPkg.RawRef.Composite(${rawRef(left)}, ${rawRef(right)})"
    case Apply(TypeApply(Select(left, TermName("compose")), List(_)), List(right)) =>
      q"$SerializationPkg.RawRef.Composite(${rawRef(right)}, ${rawRef(left)})"

    case Function(List(param), body) =>
      var refs = List.empty[Tree]
      def extract(body: Tree): Unit = body match {
        case Select(prefix, _) =>
          if (body.symbol.isTerm && body.symbol.asTerm.isCaseAccessor &&
            !definitions.TupleClass.seq.contains(prefix.tpe.typeSymbol)) {

            val primaryConstructorParams = alternatives(prefix.tpe.member(termNames.CONSTRUCTOR))
              .find(_.asMethod.isPrimaryConstructor)
              .getOrElse(abort(s"No primary constructor found for ${prefix.tpe}"))
              .typeSignature.paramLists.head

            if (primaryConstructorParams.size != 1 || !isTransparent(prefix.tpe.typeSymbol)) {
              val constrArg = primaryConstructorParams.find(_.name == body.symbol.name)
                .getOrElse(abort(s"No primary constructor parameter ${body.symbol.name} found"))

              refs ::= q"$SerializationPkg.RawRef.Field(${annotName(constrArg)})"
            }

            extract(prefix)
          } else {
            c.abort(body.pos, s"${body.symbol} is not a case class field accessor")
          }

        case MapApplyOrGet(prefix, key, baseMapSymbol) =>
          val keyType = prefix.tpe.baseType(baseMapSymbol).typeArgs.head
          refs ::= q"$SerializationPkg.RawRef.Field($SerializationPkg.GenKeyCodec.write[$keyType](${key.duplicate}))"
          extract(prefix)

        case Apply(Select(prefix, TermName("apply")), List(arg)) =>
          refs ::= rawRef(prefix)
          extract(arg)

        case Ident(_) if body.symbol == param.symbol =>
        case Apply(prefix, Nil) => extract(prefix)
        case Typed(prefix, _) => extract(prefix)
        case _ =>
          c.abort(body.pos, "This invocation can't be translated into RawRef")
      }

      extract(body)
      refs match {
        case Nil => q"$SerializationPkg.RawRef.Identity"
        case List(tree) => tree
        case _ => refs.reduce((t1, t2) => q"$SerializationPkg.RawRef.Composite($t1, $t2)")
      }

    case _ => c.abort(fun.pos, s"Expected lambda expression, GenRef or their composition")
  }

  def genRef(fun: Tree): Tree =
    q"$SerializationPkg.GenRef($fun, ${rawRef(fun)})"
}
