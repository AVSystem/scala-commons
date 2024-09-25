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

  final case class HasOptParam(
    @transientDefault flag: Boolean = false,
    @optionalParam str: Opt[String] = Opt.Empty,
  )
  object HasOptParam extends HasGenCodec[HasOptParam]
}

class IgnoreTransientDefaultMarkerTest extends AbstractCodecTest {
  import IgnoreTransientDefaultMarkerTest.*

  override type Raw = Any

  def writeToOutput(write: Output => Unit): Any = {
    var result: Any = null
    write(CustomMarkersOutputWrapper(new SimpleValueOutput(v => result = v), IgnoreTransientDefaultMarker))
    result
  }

  def createInput(raw: Any): Input =
    CustomMarkersInputWrapper(new SimpleValueInput(raw), IgnoreTransientDefaultMarker)

  test("write case class with default values") {
    testWrite(HasDefaults(str = "lol"), Map("str" -> "lol", "int" -> 42))
    testWrite(HasDefaults(43, "lol"), Map("int" -> 43, "str" -> "lol"))
    testWrite(HasDefaults(str = null), Map("str" -> null, "int" -> 42))
    testWrite(HasDefaults(str = "dafuq"), Map("str" -> "dafuq", "int" -> 42))
  }

  //noinspection RedundantDefaultArgument
  test("read case class with default values") {
    testRead(Map("str" -> "lol", "int" -> 42), HasDefaults(str = "lol", int = 42))
    testRead(Map("str" -> "lol"), HasDefaults(str = "lol", int = 42))
    testRead(Map("int" -> 43, "str" -> "lol"), HasDefaults(int = 43, str = "lol"))
    testRead(Map("str" -> null, "int" -> 42), HasDefaults(str = null, int = 42))
    testRead(Map("str" -> null), HasDefaults(str = null, int = 42))
    testRead(Map(), HasDefaults(str = "dafuq", int = 42))
  }

  test("write case class with opt values") {
    testWrite(HasOptParam(str = "lol".opt), Map("flag" -> false, "str" -> "lol"))
    testWrite(HasOptParam(), Map("flag" -> false))
  }

  //noinspection RedundantDefaultArgument
  test("write nested case class with default values") {
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
