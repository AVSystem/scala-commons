package com.avsystem.commons
package serialization

import com.avsystem.commons.mirror.name
import com.avsystem.commons.misc.HasAnnotation

opaque type TypeRepr[T] <: String = String
object TypeRepr {
  inline given [T] => TypeRepr[T] = ${ deriveImpl[T] }

  private def deriveImpl[T: Type](using quotes: Quotes): Expr[TypeRepr[T]] = {
    Expr(quotes.reflect.TypeRepr.of[T].show)
  }
}

inline def constName[T](fallback: String): String =
  HasAnnotation.get[name, T] match {
    case Some(nameAnnot) => nameAnnot.name
    case None => fallback
  }
inline def constNames[Tup <: Tuple]: Tuple.Map[Tup, [X] =>> String] =
  inline compiletime.erasedValue[Tup] match {
    case _: ((label, tpe) *: tail) =>
      val head = constName(compiletime.constValue[label].asInstanceOf[String])
      (head *: constNames[tail]).asInstanceOf[Tuple.Map[Tup, [X] =>> String]]
    case _: EmptyTuple => EmptyTuple.asInstanceOf[Tuple.Map[Tup, [X] =>> String]]
  }
