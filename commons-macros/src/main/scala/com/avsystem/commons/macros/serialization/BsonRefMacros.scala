package com.avsystem.commons
package macros.serialization

import scala.reflect.macros.blackbox

class BsonRefMacros(ctx: blackbox.Context) extends CodecMacroCommons(ctx) {

  import c.universe._

  val MongoPkg = q"$CommonsPkg.mongo"

  def bsonRef[S: c.WeakTypeTag, T: c.WeakTypeTag](fun: Tree): Tree = {
    val sType = weakTypeOf[S]
    val tType = weakTypeOf[T]
    q"$MongoPkg.BsonRef($SerializationPkg.GenRef.create[$sType].ref[$tType]($fun))"
  }
}
