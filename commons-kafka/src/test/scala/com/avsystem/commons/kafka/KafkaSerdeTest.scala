package com.avsystem.commons
package kafka

import java.io.ByteArrayOutputStream

import com.avsystem.commons.kafka.exceptions.UnsupportedVersionEvent
import com.avsystem.commons.serialization.GenCodec
import org.apache.kafka.common.serialization.Serializer
import org.scalatest.FunSuite

class KafkaSerdeTest extends FunSuite {

  sealed trait TestEvent

  case class TestEvent1(name: String, surname: String, age: Int) extends TestEvent
  case class TestEvent2(name: String, age: Int) extends TestEvent
  case class TestEvent3(name: String) extends TestEvent

  object TestEvent extends EventOps {

    implicit val registry: SerdeRegistry[TestEvent] = new SerdeRegistry[TestEvent](Map(
      1.toByte -> GenCodec.materialize[TestEvent1],
      2.toByte -> GenCodec.materialize[TestEvent2]))

    implicit val codec: GenCodec[TestEvent1] = GenCodec.materialize[TestEvent1]
    implicit val ser: Serializer[TestEvent1] = new VersionedSerializer[TestEvent1](1)
  }

  test("Serialization and deserialization test") {
    val event = TestEvent1("Martin", "Odersky", 59)

    val serialized = TestEvent.ser.serialize("topic", event)
    val deserialized = TestEvent.registry.deserialize("topic", serialized)

    assert(event == deserialized)
  }

  test("Deserialization of unsupported event should throw exception") {
    val event = TestEvent3("Martin")


    val testEvent3Serde = new KafkaSerde[TestEvent3](GenCodec.materialize[TestEvent3])
    val output = new ByteArrayOutputStream()
    output.write(3)

    val serialized = testEvent3Serde.serialize(output, event)

    assertThrows[UnsupportedVersionEvent] {
      TestEvent.registry.deserialize("topic", serialized)
    }
  }

}
