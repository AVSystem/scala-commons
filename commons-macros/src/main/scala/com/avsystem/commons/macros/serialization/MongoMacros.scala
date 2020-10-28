package com.avsystem.commons
package macros.serialization

import scala.reflect.macros.blackbox

class MongoMacros(ctx: blackbox.Context) extends CodecMacroCommons(ctx) {

  import c.universe._

  def MongoModelPkg: Tree = q"$CommonsPkg.mongo.model"
  lazy val MongoRefCls: Symbol = getType(tq"$MongoModelPkg.MongoRef[_, _]").typeSymbol

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

  def refImpl(fun: Tree): Tree = fun match {
    case Function(List(param), body) =>
      //TODO: more detailed message
      def wrongRef(tree: Tree): Nothing =
        abortAt("wrong Mongo field reference", tree.pos)

      def extractRefStep(body: Tree): Tree = body match {
        // TODO: allow body to be annotated/type-ascribed etc.
        case body: Ident if body.symbol == param.symbol =>
          c.prefix.tree

        case Select(prefix, name: TermName) =>
          val newPrefixRef = extractRefStep(prefix)
          val termSym = body.symbol.asTerm
          if (termSym.isCaseAccessor || isSealedHierarchySharedField(prefix.tpe, body.symbol.asTerm))
            q"$newPrefixRef.thisDataRef.fieldRefFor(${name.decodedName.toString})"
          else wrongRef(body)

        case _ =>
          wrongRef(body)
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
    q"${c.prefix.tree}.thisDataRef.subtypeRefFor[$cTpe]"
  }

  def isSubtype[C: c.WeakTypeTag]: Tree = {
    val cTpe = validateSubtype(weakTypeOf[C].dealias)
    q"${c.prefix.tree}.thisDataRef.subtypeConditionFor[$cTpe]"
  }
}
