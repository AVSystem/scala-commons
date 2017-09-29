package com.avsystem.commons
package kafka

import org.apache.kafka.common.serialization.{Deserializer, Serializer}

import com.avsystem.commons.jiop.JavaInterop._

import org.apache.kafka.common.serialization.Serde

abstract class AbstractSerde[T] extends Serde[T] {
  override def configure(configs: JMap[String, _], isKey: Boolean): Unit = {}

  override def close(): Unit = {}
}

abstract class AbstractSerializer[T] extends Serializer[T] {
  override def configure(configs: JMap[String, _], isKey: Boolean): Unit = {}

  override def close(): Unit = {}
}

abstract class AbstractDeserializer[T] extends Deserializer[T] {
  override def configure(configs: JMap[String, _], isKey: Boolean): Unit = {}

  override def close(): Unit = {}
}