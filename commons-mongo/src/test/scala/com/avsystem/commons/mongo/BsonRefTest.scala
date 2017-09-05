package com.avsystem.commons
package mongo

import com.avsystem.commons.serialization.{GenCodec, name, transparent}
import org.scalatest.FunSuite

case class InnerClass(
  map: Map[String, String]
)
object InnerClass {
  implicit val codec: GenCodec[InnerClass] = GenCodec.materialize
}

@transparent
case class Wrapper(s: String) extends AnyVal
object Wrapper {
  implicit val codec: GenCodec[Wrapper] = GenCodec.materialize
}

case class TestEntity(
  wrapper: Wrapper,
  @name("inner") innerClass: InnerClass
)
object TestEntity extends BsonRef.Creator[TestEntity] {
  implicit val codec: GenCodec[TestEntity] = GenCodec.materialize
}

class BsonRefTest extends FunSuite with BsonRef.Creator[TestEntity] {
  test("basic test") {
    assert(ref(_.wrapper).path === "wrapper")
    assert(ref(_.wrapper.s).path === "wrapper")
    assert(ref(_.innerClass).path === "inner")
    assert(ref(_.innerClass.map).path === "inner.map")
    assert(ref(_.innerClass.map("key")).path === "inner.map.key")
  }
}
