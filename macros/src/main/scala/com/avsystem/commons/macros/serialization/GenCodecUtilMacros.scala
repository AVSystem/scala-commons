package com.avsystem.commons
package macros.serialization

import scala.annotation.tailrec
import scala.reflect.macros.blackbox

class GenCodecUtilMacros(ctx: blackbox.Context) extends CodecMacroCommons(ctx) {
  import c.universe._

  final def Pkg: Tree = q"_root_.com.avsystem.commons.serialization"

  def codecTypeNameRaw[T: c.WeakTypeTag]: Tree =
    q"$extractName"

  def codecTypeName[T: c.WeakTypeTag]: Tree =
    q"new $Pkg.GencodecTypeName($extractName)"

  def codecFieldName[T: c.WeakTypeTag](accessor: Tree): Tree = {
    @tailrec
    def extract(tree: Tree): Name = tree match {
      case Ident(n) => n
      case Select(Select(_, _), _) => c.abort(c.enclosingPosition, s"Unsupported nested expression: $accessor")
      case Select(_, n) => n
      case Function(_, body) => extract(body)
      case Apply(func, _) => extract(func)
      case _ => c.abort(c.enclosingPosition, s"Unsupported expression: $accessor")
    }

    val name = extract(accessor)
    val tpe = weakTypeOf[T]
    val nameStr =
      applyUnapplyFor(tpe).flatMap(_.apply.asMethod.paramLists.flatten.iterator.find(_.name == name))
        .orElse(tpe.members.iterator.filter(m => m.isMethod && m.isPublic).find(_.name == name))
        .map(m => targetName(m))
        .getOrElse(c.abort(c.enclosingPosition, s"$name is not a member of $tpe"))

    q"$nameStr"
  }

  def knownSubtypesCount[T: c.WeakTypeTag]: Tree =
    q"${knownSubtypes(weakTypeOf[T]).map(_.size).getOrElse(0)}"

  private def extractName[T: c.WeakTypeTag]: String = {
    val tType = weakTypeOf[T]
    targetName(tType.dealias.typeSymbol)
  }
}
