package com.avsystem.commons
package serialization

import com.avsystem.commons.misc.{AnnotationOf, SimpleClassName}

case class SerializationName[T](name: String) extends AnyVal
object SerializationName extends SerializationNameLowPrio {
  def of[T](using sn: SerializationName[T]): String = sn.name

  given [T] => (nameAnnot: AnnotationOf[name, T]) => SerializationName[T] =
    SerializationName(nameAnnot.annot.name)
}
trait SerializationNameLowPrio { this: SerializationName.type =>
  given [T] => (SimpleClassName[T]) => SerializationName[T] =
    SerializationName(SimpleClassName.of[T])
}
