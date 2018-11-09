package com.avsystem.commons
package rpc

import com.avsystem.commons.rpc.DummyRPC._
import org.scalatest.FunSuite

class RPCMetadataTest extends FunSuite {
  case class Annot(str: String) extends MetadataAnnotation

  @Annot("on base class")
  trait Base[T] {
    def genproc(p: T): Unit

    @Annot("on base method")
    def proc(@Annot("on base param") p: String): Unit

    @rpcName("function")
    def func: Future[String]
  }
  object Base {
    implicit def AsRawReal[T: ClassTag]: AsRawRealRPC[Base[T]] = materializeAsRawReal
    implicit def metadata[T: ParamTypeMetadata]: RPCMetadata[Base[T]] = materializeMetadata
  }

  @Annot("on subclass")
  trait Sub extends Base[String] {
    @Annot("on submethod")
    def proc(@Annot("on subparam") param: String): Unit

    def getter(i: Int)(s: String): Base[String]

    def selfGetter: Sub
  }
  object Sub extends RPCCompanion[Sub]

  test("RPC metadata should be correct") {
    val m = Sub.metadata

    assert(m.name == "Sub")
    assert(m.annotations == List(Annot("on subclass"), Annot("on base class")))

    assert(m.procedureSignatures.keySet == Set("proc", "genproc"))
    assert(m.functionSignatures.keySet == Set("function"))
    assert(m.getterSignatures.keySet == Set("getter", "selfGetter"))

    assert(m.procedureSignatures("proc") == ProcedureSignature("proc",
      List(
        ParamMetadata("param", List(Annot("on subparam"), Annot("on base param")), TypeName("String"))
      ),
      List(Annot("on submethod"), Annot("on base method"))
    ))

    assert(m.procedureSignatures("genproc") == ProcedureSignature("genproc", List(
      ParamMetadata("p", Nil, TypeName("String"))
    ), Nil))

    assert(m.functionSignatures("function") == FunctionSignature("func", Nil, Nil, classTag[String]))

    val resultMetadata = m.getterSignatures("getter").resultMetadata.value
    assert(resultMetadata.annotations == List(Annot("on base class")))

    assert(resultMetadata.procedureSignatures.keySet == Set("proc", "genproc"))
    assert(resultMetadata.functionSignatures.keySet == Set("function"))
    assert(resultMetadata.getterSignatures.keySet == Set())

    assert(resultMetadata.procedureSignatures("proc") == ProcedureSignature("proc", List(
      ParamMetadata("p", List(Annot("on base param")), TypeName("String"))
    ), List(Annot("on base method"))))

    assert(m.procedureSignatures("genproc") == ProcedureSignature("genproc", List(
      ParamMetadata("p", Nil, TypeName("String"))
    ), Nil))

    assert(resultMetadata.functionSignatures("function") == FunctionSignature("func", Nil, Nil, classTag[String]))
  }
}
