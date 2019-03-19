package com.avsystem.commons
package serialization

import com.avsystem.commons.misc.{AnnotationOf, SimpleClassName}

case class SerializationName[T](name: String) extends AnyVal
object SerializationName extends SerializationNameLowPrio {
  def of[T](implicit sn: SerializationName[T]): String = sn.name

  implicit def fromNameAnnot[T](implicit nameAnnot: AnnotationOf[name, T]): SerializationName[T] =
    SerializationName(nameAnnot.annot.name)
}
trait SerializationNameLowPrio { this: SerializationName.type =>
  implicit def fromSimpleClassName[T: SimpleClassName]: SerializationName[T] =
    SerializationName(SimpleClassName.of[T])
}
