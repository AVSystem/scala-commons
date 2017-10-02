package com.avsystem.commons
package kafka

import com.avsystem.commons.kafka.exceptions.UnsupportedVersionEvent
import com.avsystem.commons.serialization.GenCodec

import scala.collection.mutable


class SerdeRegistry[T] extends AbstractDeserializer[T] {
  private val registry = mutable.Map[Byte, KafkaSerde[_ <: T]]()

  def add[X <: T](version: Byte, serde: KafkaSerde[X]): this.type = {
    registry += (version -> serde)
    this
  }

  def add[X <: T](version: Byte)(implicit codec: GenCodec[X]): this.type = {
    registry += (version -> new KafkaSerde(codec))
    this
  }

  private[kafka] def isSupported(version: Byte): Boolean = registry.keySet.contains(version)

  def apply(version: Byte): Option[KafkaSerde[_ <: T]] = registry.get(version)

  override def deserialize(topic: String, data: Array[Byte]): T = {
    registry.get(data.head) match {
      case Some(serde: KafkaSerde[T]) => serde.deserialize(topic, data.tail)
      case _ => throw new UnsupportedVersionEvent(s"Unsupported version ${data.head}, add proper serde to registry.")
    }
  }
}
