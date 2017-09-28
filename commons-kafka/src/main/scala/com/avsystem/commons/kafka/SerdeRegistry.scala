package com.avsystem.commons
package kafka

import com.avsystem.commons.kafka.exceptions.UnsupportedVersionEvent

import scala.collection.mutable


class SerdeRegistry[T] extends AbstractDeserializer[T] {
  private val registry = mutable.Map[Byte, KafkaSerde[_ <: T]]()

  def add[X <: T](version: Byte, serde: KafkaSerde[X])(implicit classTag: ClassTag[X]): SerdeRegistry[T] = {
    registry += (version -> serde)
    this
  }

  private[kafka] def isSupported(version: Byte): Boolean = registry.keySet.contains(version)

  def apply(version: Byte): Option[KafkaSerde[_ <: T]] = registry.get(version)

  override def deserialize(topic: String, data: Array[Byte]): T = {
    registry.get(data.head) match {
      case Some(serde: KafkaSerde[T]) => serde.deserializer().deserialize(topic, data.tail)
      case _ => throw new UnsupportedVersionEvent(s"Unsupported version ${data.head}, add proper serde to registry.")
    }
  }
}
