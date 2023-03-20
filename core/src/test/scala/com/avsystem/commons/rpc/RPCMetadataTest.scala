package com.avsystem.commons
package rpc

import com.avsystem.commons.misc.TypeString
import com.avsystem.commons.rpc.DummyRPC._
import com.avsystem.commons.serialization.GenCodec
import org.scalatest.funsuite.AnyFunSuite

class RPCMetadataTest extends AnyFunSuite {
  case class Annot(str: String) extends MetadataAnnotation

  @Annot("on base class")
  trait Base[T] {
    def genproc(p: T): Unit

    @Annot("on base method")
    def proc(@Annot("on base param") p: String): Unit

    @rpcName("function")
    def func[A](a: A)(implicit @encodingDependency tag: Tag[A]): Future[A]
  }
  object Base {
    implicit def codecFromTag[T: Tag]: GenCodec[T] = Tag[T].codec
    implicit def asRawReal[T: GenCodec]: AsRawRealRPC[Base[T]] = RawRPC.materializeAsRawReal
    implicit def metadata[T: TypeString]: RPCMetadata[Base[T]] = RPCMetadata.materialize
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
        ParamMetadata("param", List(Annot("on subparam"), Annot("on base param")), new TypeString("String"))
      ),
      List(Annot("on submethod"), Annot("on base method"))
    ))

    assert(m.procedureSignatures("genproc") == ProcedureSignature("genproc", List(
      ParamMetadata("p", Nil, new TypeString("String"))
    ), Nil))

    m.functionSignatures("function") uncheckedMatch {
      case FunctionSignature("func", List(TypeParamMetadata("A")), List(apm, tagpm), Nil, resTnFun) =>
        assert(resTnFun(List(classTag[String])) == classTag[String])
        apm uncheckedMatch {
          case GenericParamMetadata("a", Nil, tnFun) =>
            assert(tnFun(List(new TypeString("AAA"))) == new TypeString("AAA"))
        }
        tagpm uncheckedMatch {
          case GenericParamMetadata("tag", Nil, tnFun) =>
            assert(tnFun(List(new TypeString("AAA"))) == new TypeString("com.avsystem.commons.rpc.Tag[AAA]"))
        }
    }

    val resultMetadata = m.getterSignatures("getter").resultMetadata.value
    assert(resultMetadata.annotations == List(Annot("on base class")))

    assert(resultMetadata.procedureSignatures.keySet == Set("proc", "genproc"))
    assert(resultMetadata.functionSignatures.keySet == Set("function"))
    assert(resultMetadata.getterSignatures.keySet == Set())

    assert(resultMetadata.procedureSignatures("proc") == ProcedureSignature("proc", List(
      ParamMetadata("p", List(Annot("on base param")), new TypeString("String"))
    ), List(Annot("on base method"))))

    assert(m.procedureSignatures("genproc") == ProcedureSignature("genproc", List(
      ParamMetadata("p", Nil, new TypeString("String"))
    ), Nil))
  }
}
