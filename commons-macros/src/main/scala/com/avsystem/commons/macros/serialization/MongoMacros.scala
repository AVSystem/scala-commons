package com.avsystem.commons
package macros.serialization

import scala.reflect.macros.blackbox

class MongoMacros(ctx: blackbox.Context) extends CodecMacroCommons(ctx) {
  import c.universe._

  def MongoModelPkg: Tree = q"$CommonsPkg.mongo.model"

  // TODO: allow body to be annotated/type-ascribed etc.
  def mongoRef(fun: Tree): Tree = fun match {
    case Function(List(param), body@Select(ident, TermName(name)))
      if ident.symbol == param.symbol && body.symbol.isTerm && body.symbol.asTerm.isCaseAccessor =>
        ???
  }
}
