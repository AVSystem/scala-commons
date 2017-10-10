package com.avsystem.commons
package rpc

import com.avsystem.commons.rpc.DummyRPC._
import org.scalatest.FunSuite

class RPCMetadataTest extends FunSuite {
  case class Annot(str: String) extends MetadataAnnotation

  @RPC
  @Annot("on base class")
  trait Base[T] {
    def genproc(p: T): Unit

    @Annot("on base method")
    def proc(@Annot("on base param") p: String): Unit

    @RPCName("function")
    def func: Future[String]
  }

  @Annot("on subclass")
  trait Sub extends Base[String] {
    @Annot("on submethod")
    def proc(@Annot("on subparam") param: String): Unit

    def getter(i: Int)(s: String): Base[String]

    def selfGetter: Sub
  }

  test("RPC metadata should be correct") {
    val metadata = RPCMetadata[Sub]

    assert(metadata.name == "Sub")
    assert(metadata.annotations == List(Annot("on subclass"), Annot("on base class")))

    assert(metadata.signatures.keySet == Set("proc", "genproc", "function", "getter", "selfGetter"))

    assert(metadata.signatures("proc") == Signature("proc", List(List(
      ParamMetadata("param", List(Annot("on subparam"), Annot("on base param")), TypeName("String"))
    )), classTag[Unit], List(Annot("on submethod"), Annot("on base method"))))

    assert(metadata.signatures("genproc") == Signature("genproc", List(List(
      ParamMetadata("p", Nil, TypeName("String"))
    )), classTag[Unit], Nil))

    assert(metadata.signatures("function") == Signature("func", Nil, classTag[Future[_]], Nil))

    val resultMetadata = metadata.getterResults("getter")
    assert(resultMetadata.name == "Base")
    assert(resultMetadata.annotations == List(Annot("on base class")))

    assert(resultMetadata.signatures.keySet == Set("proc", "genproc", "function"))

    assert(resultMetadata.signatures("proc") == Signature("proc", List(List(
      ParamMetadata("p", List(Annot("on base param")), TypeName("String"))
    )), classTag[Unit], List(Annot("on base method"))))

    assert(metadata.signatures("genproc") == Signature("genproc", List(List(
      ParamMetadata("p", Nil, TypeName("String"))
    )), classTag[Unit], Nil))

    assert(resultMetadata.signatures("function") == Signature("func", Nil, classTag[Future[_]], Nil))
  }
}
