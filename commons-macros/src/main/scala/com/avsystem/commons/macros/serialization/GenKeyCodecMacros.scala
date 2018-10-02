package com.avsystem.commons
package macros.serialization

import scala.reflect.macros.blackbox

class GenKeyCodecMacros(ctx: blackbox.Context) extends CodecMacroCommons(ctx) {

  import c.universe._

  final def GenKeyCodecObj: Tree = q"$SerializationPkg.GenKeyCodec"
  final def GenKeyCodecCls: Tree = tq"$SerializationPkg.GenKeyCodec"

  def forSealedEnum[T: c.WeakTypeTag]: Tree = {
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
          def read(key: String): $tpe = key match {
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
}
