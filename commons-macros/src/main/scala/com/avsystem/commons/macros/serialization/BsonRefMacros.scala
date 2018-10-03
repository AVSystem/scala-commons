package com.avsystem.commons
package macros.serialization

import scala.reflect.macros.blackbox

class BsonRefMacros(ctx: blackbox.Context) extends CodecMacroCommons(ctx) {

  import c.universe._

  final def MongoPkg: Tree = q"$CommonsPkg.mongo"

  def bsonRef[S: WeakTypeTag, T: WeakTypeTag](fun: Tree): Tree = {
    val sType = weakTypeOf[S]
    val tType = weakTypeOf[T]
    q"$MongoPkg.BsonRef($SerializationPkg.GenRef.create[$sType].ref[$tType]($fun))"
  }
}
