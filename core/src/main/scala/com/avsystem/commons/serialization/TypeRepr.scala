package com.avsystem.commons
package serialization

opaque type TypeRepr[T] <: String = String
object TypeRepr {
  inline given [T] => TypeRepr[T] = ${ deriveImpl[T] }

  private def deriveImpl[T: Type](using quotes: Quotes): Expr[TypeRepr[T]] = {
    import quotes.reflect.*
    Expr(quotes.reflect.TypeRepr.of[T].show)
  }
}
