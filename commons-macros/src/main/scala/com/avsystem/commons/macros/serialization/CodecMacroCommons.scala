package com.avsystem.commons
package macros.serialization

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.reflect.macros.blackbox

abstract class CodecMacroCommons(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) {

  import c.universe._

  final def SerializationPkg: Tree = q"$CommonsPkg.serialization"
  final lazy val NameAnnotType = getType(tq"$SerializationPkg.name")
  final lazy val NameAnnotNameSym = NameAnnotType.member(TermName("name"))
  final lazy val WhenAbsentAnnotType = getType(tq"$SerializationPkg.whenAbsent[_]")
  final def JavaInteropObj: Tree = q"$CommonsPkg.jiop.JavaInterop"
  final def JListObj: Tree = q"$JavaInteropObj.JList"
  final def JListCls: Tree = tq"$JavaInteropObj.JList"
  final def ListBufferCls: Tree = tq"$CollectionPkg.mutable.ListBuffer"
  final def BMapCls: Tree = tq"$CollectionPkg.Map"
  final def NOptObj: Tree = q"$MiscPkg.NOpt"
  final def NOptCls: Tree = tq"$MiscPkg.NOpt"
  final def OptObj: Tree = q"$MiscPkg.Opt"
  final def OptCls: Tree = tq"$MiscPkg.Opt"
  final lazy val TransparentAnnotType = getType(tq"$SerializationPkg.transparent")
  final lazy val TransientDefaultAnnotType = getType(tq"$SerializationPkg.transientDefault")
  final lazy val FlattenAnnotType = getType(tq"$SerializationPkg.flatten")
  final lazy val OutOfOrderAnnotType = getType(tq"$SerializationPkg.outOfOrder")
  final lazy val GeneratedAnnotType = getType(tq"$SerializationPkg.generated")
  final lazy val DefaultCaseAnnotType = getType(tq"$SerializationPkg.defaultCase")
  final def GenCodecObj: Tree = q"$SerializationPkg.GenCodec"
  final def GenCodecCls: Tree = tq"$SerializationPkg.GenCodec"
  final val DefaultCaseField = "_case"

  def tupleGet(i: Int) = TermName(s"_${i + 1}")

  def targetName(sym: Symbol): String =
    findAnnotation(sym, NameAnnotType).fold(sym.name.decodedName.toString)(_.findArg[String](NameAnnotNameSym))

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

  def hasAnnotation(sym: Symbol, annotTpe: Type): Boolean =
    findAnnotation(sym, annotTpe).nonEmpty

  def isTransparent(sym: Symbol): Boolean =
    hasAnnotation(sym, TransparentAnnotType)

  def isGenerated(sym: Symbol): Boolean =
    hasAnnotation(sym, GeneratedAnnotType)
}
