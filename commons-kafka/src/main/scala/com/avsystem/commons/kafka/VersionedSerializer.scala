package com.avsystem.commons
package kafka

import com.avsystem.commons.kafka.exceptions.UnsupportedVersionEvent

class VersionedSerializer[T](version: Byte)(implicit registry: SerdeRegistry[T]) extends AbstractSerializer[T] {
  require(registry.isSupported(version), s"Version: $version isn't supported by registry. Please add it first.")
  override def serialize(topic: String, data: T): Array[Byte] = {
    registry(version) match {
      case Some(serde: KafkaSerde[T]) => version +: serde.serializer().serialize(topic, data)
      case _ => throw new UnsupportedVersionEvent(s"Unsupported version $version of ${data.getClass.getSimpleName}")
    }
  }
}
