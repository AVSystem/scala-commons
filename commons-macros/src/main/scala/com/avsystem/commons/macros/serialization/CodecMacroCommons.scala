package com.avsystem.commons
package macros.serialization

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.reflect.macros.blackbox

abstract class CodecMacroCommons(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) {

  import c.universe._

  final val SerializationPkg = q"$CommonsPackage.serialization"
  final val NameAnnotType = getType(tq"$SerializationPkg.name")
  final val JavaInteropObj = q"$CommonsPackage.jiop.JavaInterop"
  final val JListObj = q"$JavaInteropObj.JList"
  final val JListCls = tq"$JavaInteropObj.JList"
  final val ListBufferCls = tq"$CollectionPkg.mutable.ListBuffer"
  final val BMapCls = tq"$CollectionPkg.Map"
  final val NOptObj = q"$CommonsPackage.misc.NOpt"
  final val NOptCls = tq"$CommonsPackage.misc.NOpt"
  final val OptObj = q"$CommonsPackage.misc.Opt"
  final val OptCls = tq"$CommonsPackage.misc.Opt"
  final val TransparentAnnotType = getType(tq"$SerializationPkg.transparent")
  final val TransientDefaultAnnotType = getType(tq"$SerializationPkg.transientDefault")
  final val FlattenAnnotType = getType(tq"$SerializationPkg.flatten")
  final val GenCodecObj = q"$SerializationPkg.GenCodec"
  final val GenCodecCls = tq"$SerializationPkg.GenCodec"
  final val CaseField = "_case"

  def tupleGet(i: Int) = TermName(s"_${i + 1}")

  def targetName(sym: Symbol): String =
    getAnnotations(sym, NameAnnotType).headOption.map(_.tree.children.tail).map {
      case Literal(Constant(str: String)) :: _ => str
      case param :: _ => c.abort(param.pos, s"@name argument must be a string literal")
    }.getOrElse(sym.name.decodedName.toString)

  def getAnnotations(sym: Symbol, annotTpe: Type): List[Annotation] = {
    val syms =
      if (sym.isClass) sym.asClass.baseClasses
      else sym :: sym.overrides
    syms.flatMap(_.annotations).filter(_.tree.tpe <:< annotTpe)
  }

  def hasAnnotation(sym: Symbol, annotTpe: Type): Boolean =
    getAnnotations(sym, annotTpe).nonEmpty

  def isTransparent(sym: Symbol): Boolean =
    hasAnnotation(sym, TransparentAnnotType)
}