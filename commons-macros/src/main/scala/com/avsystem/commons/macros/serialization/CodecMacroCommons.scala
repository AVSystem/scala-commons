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
  final val OutOfOrderAnnotType = getType(tq"$SerializationPkg.outOfOrder")
  final val GeneratedAnnotType = getType(tq"$SerializationPkg.generated")
  final val DefaultCaseAnnotType = getType(tq"$SerializationPkg.defaultCase")
  final val GenCodecObj = q"$SerializationPkg.GenCodec"
  final val GenCodecCls = tq"$SerializationPkg.GenCodec"
  final val DefaultCaseField = "_case"

  def tupleGet(i: Int) = TermName(s"_${i + 1}")

  def targetName(sym: Symbol): String =
    getAnnotations(sym, NameAnnotType).headOption.map(_.children.tail).map {
      case StringLiteral(str) :: _ => str
      case p :: _ => c.abort(p.pos, s"@name argument must be a string literal")
    }.getOrElse(sym.name.decodedName.toString)

  def caseAccessorFor(sym: Symbol): Symbol =
    if (sym.isParameter && sym.owner.isConstructor) {
      val ownerClass = sym.owner.owner.asClass
      if (ownerClass.isCaseClass) {
        alternatives(ownerClass.toType.member(sym.name)).find(_.asTerm.isCaseAccessor).getOrElse(NoSymbol)
      } else NoSymbol
    } else NoSymbol

  def withAccessed(sym: Symbol): List[Symbol] =
    if (sym.isTerm) {
      val tsym = sym.asTerm
      if (tsym.isGetter) List(sym, tsym.accessed)
      else List(sym)
    } else List(sym)

  def getAnnotations(sym: Symbol, annotTpe: Type): List[Tree] = {
    val caseAccessor = caseAccessorFor(sym)
    val syms =
      if (caseAccessor != NoSymbol) sym :: caseAccessor :: caseAccessor.overrides
      else if (sym.isClass) sym.asClass.baseClasses
      else sym :: sym.overrides
    syms.flatMap(s => withAccessed(s).flatMap(s =>
      s.annotations.flatMap(a => a.tree :: aggregatedAnnotations(a.tree)).filter(_.tpe <:< annotTpe)))
  }

  def hasAnnotation(sym: Symbol, annotTpe: Type): Boolean =
    getAnnotations(sym, annotTpe).nonEmpty

  def isTransparent(sym: Symbol): Boolean =
    hasAnnotation(sym, TransparentAnnotType)

  def isGenerated(sym: Symbol): Boolean =
    hasAnnotation(sym, GeneratedAnnotType)
}
