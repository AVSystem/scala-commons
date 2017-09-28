package com.avsystem.commons
package kafka

import com.avsystem.commons.kafka.exceptions.UnsupportedVersionEvent
import com.avsystem.commons.serialization.GenCodec
import org.apache.kafka.common.serialization.Serializer
import org.scalatest.FunSuite

class KafkaSerdeTest extends FunSuite {

  trait TestEvent

  case class TestEvent1(name: String, surname: String, age: Int) extends TestEvent
  case class TestEvent2(name: String, age: Int) extends TestEvent
  case class TestEvent3(name: String) extends TestEvent

  object TestEvent extends EventOps {

    implicit  val registry: SerdeRegistry[TestEvent] = new SerdeRegistry[TestEvent]()
      .add(1.toByte, new KafkaSerde[TestEvent1](GenCodec.materialize[TestEvent1]))
      .add(2.toByte, new KafkaSerde[TestEvent2](GenCodec.materialize[TestEvent2]))

    implicit val ser: Serializer[TestEvent] = new VersionedSerializer[TestEvent](1)
  }

  test("Serialization and deserialization test") {
    val event = TestEvent1("Martin", "Odersky", 59)

    val serialized = TestEvent.ser.serialize("topic", event)
    val deserialized = TestEvent.registry.deserialize("topic", serialized)

    assert(event == deserialized)
  }

  test("Serialization of unsupported event should throw exception") {
    val event = TestEvent3("Martin")

    assertThrows[ClassCastException]{
      TestEvent.ser.serialize("topic", event)
    }
  }

  test("Deserialization of unsupported event should throw exception") {
    val event = TestEvent3("Martin")
    val testEvent3Serde = new KafkaSerde[TestEvent3](GenCodec.materialize[TestEvent3])

    val serialized = testEvent3Serde.serializer().serialize("topic", event)

    assertThrows[UnsupportedVersionEvent] {
      TestEvent.registry.deserialize("topic", serialized)
    }
  }

  test("Invalid serializer should throw exception") {
    assertThrows[IllegalArgumentException] {
      new VersionedSerializer[TestEvent](5)
    }
  }

}
