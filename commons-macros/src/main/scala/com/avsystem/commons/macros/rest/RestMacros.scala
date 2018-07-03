package com.avsystem.commons
package macros.rest

import com.avsystem.commons.macros.AbstractMacroCommons

import scala.reflect.macros.blackbox

class RestMacros(val ctx: blackbox.Context) extends AbstractMacroCommons(ctx) {

  import c.universe._

  val RestPkg: Tree = q"$CommonsPkg.rest"
  val RawRestObj: Tree = q"$RestPkg.RawRest"
  val RestMetadataObj: Tree = q"$RestPkg.RestMetadata"
  val RestMetadataCls: Tree = tq"$RestPkg.RestMetadata"

  def instances[Real: WeakTypeTag]: Tree = {
    val realTpe = weakTypeOf[Real]
    val instancesTpe = c.macroApplication.tpe

    val asRawTpe: Type = getType(tq"$RawRestObj.AsRawRpc[$realTpe]")
    val asRealTpe: Type = getType(tq"$RawRestObj.AsRealRpc[$realTpe]")
    val asRawRealTpe: Type = getType(tq"$RawRestObj.AsRawRealRpc[$realTpe]")
    val metadataTpe: Type = getType(tq"$RestMetadataCls[$realTpe]")

    val memberDefs = instancesTpe.members.iterator.filter(m => m.isAbstract && m.isMethod).map { m =>
      val resultTpe = m.typeSignatureIn(instancesTpe).finalResultType
      val body =
        if (resultTpe <:< asRawRealTpe) q"$RawRestObj.materializeAsRawReal[$realTpe]"
        else if (resultTpe <:< asRawTpe) q"$RawRestObj.materializeAsRaw[$realTpe]"
        else if (resultTpe <:< asRealTpe) q"$RawRestObj.materializeAsReal[$realTpe]"
        else if (resultTpe <:< metadataTpe) q"$RestMetadataObj.materializeForRpc[$realTpe]"
        else abort(s"Unexpected result type: $resultTpe")
      q"lazy val ${m.name.toTermName} = $body"
    }.toList

    q"new $instancesTpe { ..$memberDefs }"
  }
}
