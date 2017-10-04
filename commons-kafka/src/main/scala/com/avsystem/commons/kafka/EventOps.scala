package com.avsystem.commons
package kafka

import org.apache.kafka.common.serialization.{Deserializer, Serde, Serializer}

trait EventOps {
  def serde[T](implicit registry: SerdeRegistry[T], ser: Serializer[T]):Serde[T] = new AbstractSerde[T] {
    override def deserializer(): Deserializer[T] = registry
    override def serializer(): Serializer[T] = ser
  }
}
