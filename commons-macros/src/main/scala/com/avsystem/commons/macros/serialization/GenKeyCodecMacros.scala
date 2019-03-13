package com.avsystem.commons
package macros.serialization

import scala.reflect.macros.blackbox

class GenKeyCodecMacros(ctx: blackbox.Context) extends CodecMacroCommons(ctx) {

  import c.universe._

  final def GenKeyCodecObj: Tree = q"$SerializationPkg.GenKeyCodec"
  final def GenKeyCodecCls: Tree = tq"$SerializationPkg.GenKeyCodec"

  def forSealedEnum[T: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T]
    knownSubtypes(tpe).map { subtypes =>
      def singleValue(st: Type): Tree = singleValueFor(st).getOrElse(abort(s"$st is not an object"))
      val nameBySym = subtypes.groupBy(st => targetName(st.typeSymbol)).map {
        case (name, List(subtype)) => (subtype.typeSymbol, name)
        case (name, kst) =>
          abort(s"Objects ${kst.map(_.typeSymbol.name).mkString(", ")} have the same @name: $name")
      }

      q"""
        new $GenKeyCodecCls[$tpe] {
          def tpeString = ${tpe.toString}
          def read(key: $StringCls): $tpe = key match {
            case ..${subtypes.map(st => cq"${nameBySym(st.typeSymbol)} => ${singleValue(st)}")}
            case _ => throw new $SerializationPkg.GenCodec.ReadFailure(s"Cannot read $$tpeString, unknown object: $$key")
          }
          def write(value: $tpe): String = value match {
            case ..${subtypes.map(st => cq"_: $st => ${nameBySym(st.typeSymbol)}")}
          }
        }
      """
    }.getOrElse(abort(s"$tpe is not a sealed trait or class"))
  }

  def forTransparentWrapper[T: WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[T].dealias
    val codecTpe = getType(tq"$GenKeyCodecCls[$tpe]")
    val (applyUnapply, param) = applyUnapplyFor(tpe) match {
      case Some(au@ApplyUnapply(_, _, _, _, List(soleParam))) => (au, soleParam)
      case _ => abort(s"$tpe is not a case class (or case class-like type) with exactly one field")
    }

    val wrappedCodecTpe = getType(tq"$GenKeyCodecCls[${param.typeSignature}]")
    val clue = s"Cannot materialize $codecTpe because of problem with parameter ${param.name}:\n"
    val wrappedCodec = inferCachedImplicit(wrappedCodecTpe, clue, param.pos)

    val unwrapped =
      if (applyUnapply.standardCaseClass)
        q"value.${param.name.toTermName}"
      else
        q"""
          ${applyUnapply.typedCompanion}.unapply[..${tpe.typeArgs}](value)
            .getOrElse(throw new $SerializationPkg.GenCodec.WriteFailure(
              s"Cannot write $$tpeString, unapply failed for $$value"))
         """

    q"""
      new $codecTpe {
        ..$cachedImplicitDeclarations
        def tpeString: $StringCls = ${tpe.toString}
        def read(key: $StringCls): $tpe = ${applyUnapply.mkApply(List(q"$wrappedCodec.read(key)"))}
        def write(value: $tpe): $StringCls = $wrappedCodec.write($unwrapped)
      }
     """
  }
}
