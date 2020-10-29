package com.avsystem.commons
package macros.serialization

import scala.reflect.macros.blackbox

class MongoMacros(ctx: blackbox.Context) extends CodecMacroCommons(ctx) {

  import c.universe._

  def MongoModelPkg: Tree = q"$CommonsPkg.mongo.model"
  lazy val MongoRefCls: Symbol = getType(tq"$MongoModelPkg.MongoRef[_, _]").typeSymbol
  lazy val SeqApplySym: Symbol = typeOf[scala.collection.Seq[Any]].member(TermName("apply"))
  lazy val SeqHeadRef: Symbol = typeOf[scala.collection.Seq[Any]].member(TermName("head"))
  lazy val MapApplySym: Symbol = typeOf[scala.collection.Map[Any, Any]].member(TermName("apply"))

  // check if some symbol is an abstract method of a sealed trait/class implemented in every case class
  // by a field with exactly the same type
  private def isSealedHierarchySharedField(ownerTpe: Type, sym: TermSymbol): Boolean = {
    isSealedHierarchyRoot(ownerTpe.typeSymbol) && sym.isMethod && sym.isAbstract && {
      val sig = sym.typeSignatureIn(ownerTpe)
      sig.typeParams.isEmpty && sig.paramLists.isEmpty &&
        knownSubtypes(ownerTpe).exists { subtypes =>
          subtypes.forall { subtype =>
            alternatives(subtype.member(sym.name)).exists { subMember =>
              subMember.asTerm.isCaseAccessor &&
                subMember.typeSignatureIn(subtype).finalResultType =:= sig.finalResultType
            }
          }
        }
    }
  }

  def isOptionLike(fullTpe: Type, wrappedTpe: Type): Boolean =
    fullTpe != null && fullTpe != NoType && wrappedTpe != null && wrappedTpe != NoType &&
      c.inferImplicitValue(getType(tq"$CommonsPkg.meta.OptionLike.Aux[$fullTpe, $wrappedTpe]")) != EmptyTree

  def overridesAnyOf(sym: Symbol, superSymbols: Symbol*): Boolean = {
    val overrides = sym :: sym.overrides
    overrides.exists(superSymbols.contains)
  }

  def refImpl(fun: Tree): Tree = fun match {
    case Function(List(param), body) =>
      //TODO: more detailed message
      def invalid(tree: Tree): Nothing =
        c.abort(tree.pos, "invalid MongoDB field reference")

      def extractRefStep(body: Tree): Tree = body match {
        // TODO: allow body to be annotated/type-ascribed etc.
        case body: Ident if body.symbol == param.symbol =>
          c.prefix.tree

        case Select(prefix, name: TermName) =>
          val newPrefixRef = extractRefStep(prefix)
          val termSym = body.symbol.asTerm
          val prefixTpe = prefix.tpe.widen
          val bodyTpe = body.tpe.widen

          if (termSym.isCaseAccessor || isSealedHierarchySharedField(prefixTpe, body.symbol.asTerm))
            q"$newPrefixRef.asAdtRef.fieldRefFor[$bodyTpe](${name.decodedName.toString})"
          else if (name == TermName("get") && isOptionLike(prefixTpe, bodyTpe))
            q"$newPrefixRef.get"
          else if (name == TermName("head") && overridesAnyOf(body.symbol, SeqHeadRef))
            q"$newPrefixRef.head"
          else
            invalid(body)

        case Apply(Select(prefix, TermName("apply")), List(argument))
          if overridesAnyOf(body.symbol, SeqApplySym, MapApplySym) =>

          q"${extractRefStep(prefix)}.apply($argument)"

        case _ =>
          invalid(body)
      }

      extractRefStep(body)

    case _ =>
      abortAt("wrong mongo reference lambda", fun.pos)
  }

  private def validateSubtype(tpe: Type): Type = {
    val cSym = tpe.typeSymbol
    if (!cSym.isClass || (!cSym.asClass.isSealed && cSym.isAbstract)) {
      abort(s"$tpe is not a case class/object or intermediate sealed trait/class")
    }
    tpe
  }

  def asSubtype[C: c.WeakTypeTag]: Tree = {
    val cTpe = validateSubtype(weakTypeOf[C].dealias)
    q"${c.prefix.tree}.asAdtRef.subtypeRefFor[$cTpe]"
  }

  def isSubtype[C: c.WeakTypeTag]: Tree = {
    val cTpe = validateSubtype(weakTypeOf[C].dealias)
    q"${c.prefix.tree}.asAdtRef.subtypeConditionFor[$cTpe]"
  }
}
