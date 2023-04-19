package com.avsystem.commons
package macros.serialization

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.reflect.macros.blackbox

abstract class CodecMacroCommons(ctx: blackbox.Context) extends AbstractMacroCommons(ctx) {

  import c.universe._

  final def SerializationPkg: Tree = q"$CommonsPkg.serialization"
  final lazy val NameAnnotType = staticType(tq"$SerializationPkg.name")
  final lazy val NameAnnotNameSym = NameAnnotType.member(TermName("name"))
  final lazy val WhenAbsentAnnotType = staticType(tq"$SerializationPkg.whenAbsent[_]")
  final def JavaInteropObj: Tree = q"$CommonsPkg.jiop.JavaInterop"
  final def JListObj: Tree = q"$JavaInteropObj.JList"
  final def JListCls: Tree = tq"$JavaInteropObj.JList"
  final def ListBufferCls: Tree = tq"$CollectionPkg.mutable.ListBuffer"
  final def BMapCls: Tree = tq"$CollectionPkg.Map"
  final def NOptObj: Tree = q"$MiscPkg.NOpt"
  final def NOptCls: Tree = tq"$MiscPkg.NOpt"
  final def OptObj: Tree = q"$CommonsPkg.Opt"
  final def OptCls: Tree = tq"$CommonsPkg.Opt"
  final lazy val TransparentAnnotType = staticType(tq"$SerializationPkg.transparent")
  final lazy val TransientDefaultAnnotType = staticType(tq"$SerializationPkg.transientDefault")
  final lazy val FlattenAnnotType = staticType(tq"$SerializationPkg.flatten")
  final lazy val OutOfOrderAnnotType = staticType(tq"$SerializationPkg.outOfOrder")
  final lazy val GeneratedAnnotType = staticType(tq"$SerializationPkg.generated")
  final lazy val DefaultCaseAnnotType = staticType(tq"$SerializationPkg.defaultCase")
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

  def isGenerated(sym: Symbol): Boolean = sym.isTerm && {
    val ts = sym.asTerm
    // do not treat val/var's underlying field as a generated member,
    // pretend that the annotation is actually applied on its getter
    (ts.getter == ts || ts.getter == NoSymbol) && hasAnnotation(ts, GeneratedAnnotType) ||
      ts.isGetter && hasAnnotation(ts.accessed, GeneratedAnnotType)
  }
}
