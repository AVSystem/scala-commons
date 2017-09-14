package com.avsystem.commons
package macros.serialization

import scala.reflect.macros.blackbox

class BsonRefMacros(ctx: blackbox.Context) extends CodecMacroCommons(ctx) {

  import c.universe._

  val MongoPkg = q"$CommonsPackage.mongo"

  def bsonRef[S: c.WeakTypeTag](fun: Tree): Tree = {
    val sType = weakTypeOf[S]
    q"$MongoPkg.BsonRef($SerializationPkg.RawRef[$sType].apply($fun))"
  }
}
