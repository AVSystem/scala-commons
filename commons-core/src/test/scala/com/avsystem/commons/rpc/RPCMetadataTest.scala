package com.avsystem.commons
package rpc

import org.scalatest.FunSuite

import scala.concurrent.Future

/**
  * Author: ghik
  * Created: 25/02/16.
  */
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

    def getter(i: Int)(s: String): Base

    def selfGetter: Sub
  }

  test("RPC metadata should be correct") {
    val metadata = RPCMetadata[Sub]

    assert(metadata.name == "Sub")
    assert(metadata.annotations == List(Annot("on subclass"), Annot("on base class")))

    assert(metadata.signatures.keySet == Set("proc", "function", "getter"))

    assert(metadata.signatures("proc") == Signature("proc", List(List(
      ParamMetadata("param", List(Annot("on subparam"), Annot("on base param")))
    )), List(Annot("on submethod"), Annot("on base method"))))

    assert(metadata.signatures("function") == Signature("func", Nil, Nil))

    val resultMetadata = metadata.getterResults("getter")
    assert(resultMetadata.name == "Base")
    assert(resultMetadata.annotations == List(Annot("on base class")))

    assert(resultMetadata.signatures.keySet == Set("proc", "function"))

    assert(resultMetadata.signatures("proc") == Signature("proc", List(List(
    ParamMetadata("p", List(Annot("on base param")))
    )), List(Annot("on base method"))))

    assert(resultMetadata.signatures("function") == Signature("func", Nil, Nil))
  }
}
