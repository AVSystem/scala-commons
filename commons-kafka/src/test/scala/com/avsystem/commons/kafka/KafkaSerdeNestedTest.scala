package com.avsystem.commons
package kafka

import java.util.Date

import com.avsystem.commons.serialization.GenCodec
import org.apache.kafka.common.serialization.Serializer
import org.scalatest.FunSuite

class KafkaSerdeNestedTest extends FunSuite {

  trait TestEvent


  case class Dog(name: String, age: Int)

  case class Owner(name: String, surname: String)

  case class TestEvent1(timestamp: JDate, owner: Owner, pet: Dog) extends TestEvent

  object TestEvent extends EventOps {

    implicit val registry: SerdeRegistry[TestEvent] = new SerdeRegistry[TestEvent]()
      .add(1, new KafkaSerde[TestEvent1](GenCodec.materializeRecursively[TestEvent1]))

    implicit val ser: Serializer[TestEvent] = new VersionedSerializer[TestEvent](1)

  }

  test("Nested case classes serialization and deserialization") {

    val event = TestEvent1(new Date(), Owner("Martin", "Odersky"), Dog("Sparky", 2))

    val serialized = TestEvent.ser.serialize("topic", event)
    val deserialized = TestEvent.registry.deserialize("topic", serialized)

    assert(event == deserialized)
  }

}
