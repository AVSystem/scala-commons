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
  val TransparentGets = Set(
    getType(tq"$CommonsPackage.misc.Opt[_]"),
    getType(tq"$CommonsPackage.misc.OptArg[_]"),
    getType(tq"$CommonsPackage.misc.OptRef[_]")
  ).map(_.member(TermName("get")))

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

  private def primaryConstructorParamFor(tpe: Type, accessor: Symbol): Symbol =
    alternatives(tpe.member(termNames.CONSTRUCTOR))
      .find(_.asMethod.isPrimaryConstructor)
      .map(_.typeSignature.paramLists.head)
      .flatMap(_.find(_.name == accessor.name))
      .getOrElse(abort(s"Could not find primary constructor parameter ${accessor.name}"))

  def rawRef(fun: Tree): Tree = fun match {
    case genRef if genRef.tpe <:< GenRefTpe => q"${genRef.duplicate}.rawRef"
    case Apply(TypeApply(Select(left, TermName("andThen")), List(_)), List(right)) =>
      q"$SerializationPkg.RawRef.Composite(${rawRef(left)}, ${rawRef(right)})"
    case Apply(TypeApply(Select(left, TermName("compose")), List(_)), List(right)) =>
      q"$SerializationPkg.RawRef.Composite(${rawRef(right)}, ${rawRef(left)})"

    case Function(List(param), body) =>
      var refs = List.empty[Tree]

      def extract(body: Tree): Unit = body match {
        case Select(prefix, _) if TransparentGets.contains(body.symbol) =>
          extract(prefix)

        case Select(prefix, _) =>
          val prefixTpe = prefix.tpe.widen
          val bodyTpe = body.tpe.widen
          val prefixTpeSym = prefixTpe.typeSymbol
          val selSym = body.symbol

          def fieldMemberFor(tpe: Type, member: Symbol): Symbol =
            if (hasAnnotation(member, GeneratedAnnotType))
              member
            else if (member.isTerm && member.asTerm.isCaseAccessor && !isTuple(prefixTpeSym))
              primaryConstructorParamFor(tpe, member)
            else
              c.abort(body.pos, s"$member in $tpe is neither a case class field accessor nor a @generated member")

          knownSubtypes(prefixTpe) match {
            case Some(subtypes) if subtypes.nonEmpty && hasAnnotation(prefixTpeSym, FlattenAnnotType) =>
              val subMembers = subtypes.map { subtype =>
                val sym = alternatives(subtype.member(selSym.name))
                  .find(s => s == selSym || s.overrides.contains(selSym))
                  .getOrElse(c.abort(body.pos, s"Could not find overriding member for $selSym in $subtype"))
                (subtype, sym)
              }

              val fieldSymbols = subMembers.map { case (subtype, subMember) =>
                val fieldSym = fieldMemberFor(subtype, subMember)
                val fieldType = actualParamType(fieldSym.typeSignatureIn(subtype).finalResultType)
                if (!(fieldType =:= bodyTpe)) {
                  c.abort(body.pos, s"$subMember in $subtype has different type ($fieldType) than $selSym in $prefixTpe ($bodyTpe)")
                }
                fieldSym
              }

              val memberName = targetName(fieldSymbols.head)
              if (fieldSymbols.exists(s => targetName(s) != memberName)) {
                c.abort(body.pos, s"All members that override $selSym in $prefixTpe in subtypes must have the same @name")
              }

              refs ::= q"$SerializationPkg.RawRef.Field($memberName)"

            case _ =>
              val fieldSym = fieldMemberFor(prefixTpe, selSym)
              if (!isTransparent(prefixTpeSym)) {
                refs ::= q"$SerializationPkg.RawRef.Field(${targetName(fieldSym)})"
              }
          }
          extract(prefix)

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
