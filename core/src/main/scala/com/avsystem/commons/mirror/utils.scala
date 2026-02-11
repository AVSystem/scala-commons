package com.avsystem.commons
package mirror

extension (comp: Expr.type) def ofOption[A: Type](opt: Option[Expr[A]])(using Quotes): Expr[Option[A]] = opt match {
  case Some(expr) => '{ Some($expr) }
  case None => '{ None }
}
