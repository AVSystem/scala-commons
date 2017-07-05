package com.avsystem.commons
package macros.serialization

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.reflect.macros.blackbox

abstract class CodecMacroCommons(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) {

  import c.universe._

  val SerializationPkg = q"$CommonsPackage.serialization"
  val NameAnnotType = getType(tq"$SerializationPkg.name")
  val JavaInteropObj = q"$CommonsPackage.jiop.JavaInterop"
  val JListObj = q"$JavaInteropObj.JList"
  val JListCls = tq"$JavaInteropObj.JList"
  val ListBufferCls = tq"$CollectionPkg.mutable.ListBuffer"
  val BMapCls = tq"$CollectionPkg.Map"
  val NOptObj = q"$CommonsPackage.misc.NOpt"
  val NOptCls = tq"$CommonsPackage.misc.NOpt"

  def tupleGet(i: Int) = TermName(s"_${i + 1}")

  def annotName(sym: Symbol): String =
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
}