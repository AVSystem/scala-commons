package com.avsystem.commons
package serialization

import com.avsystem.commons.serialization.CodecTestData.HasDefaults

object IgnoreTransientDefaultMarkerTest {
  final case class NestedHasDefaults(
    @transientDefault flag: Boolean = false,
    obj: HasDefaults,
    list: Seq[HasDefaults],
    @transientDefault defaultObj: HasDefaults = HasDefaults(),
  )
  object NestedHasDefaults extends HasGenCodec[NestedHasDefaults]
}

class IgnoreTransientDefaultMarkerTest extends AbstractCodecTest {
  import IgnoreTransientDefaultMarkerTest._

  override type Raw = Any

  def writeToOutput(write: Output => Unit): Any = {
    var result: Any = null
    write(CustomMarkersOutputWrapper(new SimpleValueOutput(result = _), IgnoreTransientDefaultMarker))
    result
  }

  def createInput(raw: Any): Input = new SimpleValueInput(raw)

  test("case class with default values") {
    testWrite(HasDefaults(str = "lol"), Map("str" -> "lol", "int" -> 42))
    testWrite(HasDefaults(43, "lol"), Map("int" -> 43, "str" -> "lol"))
    testWrite(HasDefaults(str = null), Map("str" -> null, "int" -> 42))
    testWrite(HasDefaults(str = "dafuq"), Map("str" -> "dafuq", "int" -> 42))
  }

  test("nested case class with default values") {
    testWrite(
      value = NestedHasDefaults(
        flag = false,
        obj = HasDefaults(str = "lol"),
        list = Seq(HasDefaults(int = 43)),
        defaultObj = HasDefaults(),
      ),
      expectedRepr = Map(
        "flag" -> false,
        "defaultObj" -> Map[String, Any]("str" -> "kek", "int" -> 42),
        "obj" -> Map[String, Any]("str" -> "lol", "int" -> 42),
        "list" -> List(Map[String, Any]("str" -> "kek", "int" -> 43)),
      ),
    )
  }
}
