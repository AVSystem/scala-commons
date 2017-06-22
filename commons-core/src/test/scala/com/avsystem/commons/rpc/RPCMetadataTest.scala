package com.avsystem.commons
package rpc

import org.scalatest.FunSuite


class RPCMetadataTest extends FunSuite {
  case class Annot(str: String) extends MetadataAnnotation

  @RPC
  @Annot("on base class")
  trait Base {
    @Annot("on base method")
    def proc(@Annot("on base param") p: String): Unit

    @RPCName("function")
    def func: Future[String]
  }

  @Annot("on subclass")
  trait Sub extends Base {
    @Annot("on submethod")
    def proc(@Annot("on subparam") param: String): Unit

    def manyTypes(d: Double, f: Float, l: Long, i: Int, c: Char, sh: Short, b: Byte, s: String, obj: Base): Unit

    def getter(i: Int)(s: String): Base

    def selfGetter: Sub
  }

  test("RPC metadata should be correct") {
    val metadata = RPCMetadata[Sub]

    assert(metadata.name == "Sub")
    assert(metadata.annotations == List(Annot("on subclass"), Annot("on base class")))

    assert(metadata.signatures.keySet == Set("proc", "function", "getter", "selfGetter", "manyTypes"))

    assert(metadata.signatures("proc") == Signature("proc", List(List(
      ParamMetadata("param", ParamMetadata.StringType, List(Annot("on subparam"), Annot("on base param")))
    )), List(Annot("on submethod"), Annot("on base method"))))

    assert(metadata.signatures("function") == Signature("func", Nil, Nil))

    val resultMetadata = metadata.getterResults("getter")
    assert(resultMetadata.name == "Base")
    assert(resultMetadata.annotations == List(Annot("on base class")))
    assert(metadata.signatures("getter").paramMetadata == List(
      List(ParamMetadata("i", ParamMetadata.IntType, List())),
      List(ParamMetadata("s", ParamMetadata.StringType, List()))
    ))

    assert(metadata.signatures("manyTypes").paramMetadata == List(List(
      ParamMetadata("d", ParamMetadata.DoubleType, List()),
      ParamMetadata("f", ParamMetadata.FloatType, List()),
      ParamMetadata("l", ParamMetadata.LongType, List()),
      ParamMetadata("i", ParamMetadata.IntType, List()),
      ParamMetadata("c", ParamMetadata.CharType, List()),
      ParamMetadata("sh", ParamMetadata.ShortType, List()),
      ParamMetadata("b", ParamMetadata.ByteType, List()),
      ParamMetadata("s", ParamMetadata.StringType, List()),
      ParamMetadata("obj", ParamMetadata.ObjectType, List())
    )))

    assert(resultMetadata.signatures.keySet == Set("proc", "function"))

    assert(resultMetadata.signatures("proc") == Signature("proc", List(List(
      ParamMetadata("p", ParamMetadata.StringType, List(Annot("on base param")))
    )), List(Annot("on base method"))))

    assert(resultMetadata.signatures("function") == Signature("func", Nil, Nil))
  }
}
