package com.avsystem.commons
package serialization

import com.avsystem.commons.misc.HasAnnotation

opaque type TypeRepr[T] <: String = String
object TypeRepr {
  inline given [T] => TypeRepr[T] = ${ deriveImpl[T] }

  private def deriveImpl[T: Type](using quotes: Quotes): Expr[TypeRepr[T]] = {
    import quotes.reflect.*
    Expr(quotes.reflect.TypeRepr.of[T].show)
  }
}

inline def constName[T](fallback: String): String = compiletime.summonFrom {
  case h: HasAnnotation[`name`, T] => h.name
  case _ => fallback
}
inline def constNames[Tup <: Tuple]: Tuple.Map[Tup, [X] =>> String] =
  inline compiletime.erasedValue[Tup] match {
    case _: ((label, tpe) *: tail) =>
      val head = constName(compiletime.constValue[label].asInstanceOf[String])
      (head *: constNames[tail]).asInstanceOf[Tuple.Map[Tup, [X] =>> String]]
    case _: EmptyTuple => EmptyTuple.asInstanceOf[Tuple.Map[Tup, [X] =>> String]]
  }
