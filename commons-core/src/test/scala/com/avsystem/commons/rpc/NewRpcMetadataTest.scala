package com.avsystem.commons
package rpc

import com.avsystem.commons.meta.infer
import com.avsystem.commons.misc.TypeString
import com.avsystem.commons.serialization.{GenCodec, transientDefault, whenAbsent}
import org.scalatest.FunSuite

class td extends transientDefault

trait SomeBase {
  def difolt: Boolean = true

  @POST def postit(arg: String, @header("X-Bar") bar: String, int: Int, @header("X-Foo") @suchMeta(2, "b") foo: String): String
}

trait Box[T]
object Box {
  implicit def codec[T: GenCodec]: GenCodec[Box[T]] = ???
}

class annotTypeString[T](@infer val ts: TypeString[T] = RpcMetadata.auto) extends StaticAnnotation

trait TestApi extends SomeBase {
  @filter def doSomething(@filter double: Double): String
  def doSomethingElse(double: Double): String
  def varargsMethod(krap: String, dubl: Double)(czy: Boolean, @renamed(42, "nejm") ints: Int*): Future[Unit]
  def defaultValueMethod(@td int: Int = 0, @whenAbsent(difolt) bul: Boolean): Future[Unit]
  def flames(arg: String, otherArg: => Int, varargsy: Double*): Unit
  def overload(@td int: Int = 42): Unit
  @rpcName("ovgetter") def overload(lel: String): TestApi
  @rpcName("ovprefix") def overload: TestApi
  def getit(stuff: String, @suchMeta(1, "a") otherStuff: List[Int]): TestApi
  def postit(arg: String, bar: String, int: Int, @suchMeta(3, "c") foo: String): String
  def generyk[T](@annotTypeString[Box[T]] lel: Box[T])(implicit @encodingDependency tag: Tag[T]): Future[List[T]]
}
object TestApi {
  implicit def codecFromTag[T: Tag]: GenCodec[T] = Tag[T].codec
  implicit def asRawRealFromGenCodec[T: GenCodec]: AsRawReal[String, T] = ???
  implicit def futureAsRawRealFromGenCodec[T: GenCodec]: AsRawReal[Future[String], Future[T]] = ???

  implicit val asRaw: NewRawRpc.AsRawRpc[TestApi] = NewRawRpc.materializeAsRaw[TestApi]
  implicit val asReal: NewRawRpc.AsRealRpc[TestApi] = NewRawRpc.materializeAsReal[TestApi]
  implicit val metadata: NewRpcMetadata[TestApi] = NewRpcMetadata.materialize[TestApi]
  implicit val partialMetadata: PartialMetadata[TestApi] = PartialMetadata.materialize[TestApi]
}

class NewRpcMetadataTest extends FunSuite {
  test("TestApi metadata") {
    assert(TestApi.metadata.toString ==
      """com.avsystem.commons.rpc.TestApi
        |  DO SOMETHING ELSE: true
        |  PROCEDURES:
        |  flames -> def flames: void
        |    NO AJDI
        |    ARGS:
        |    arg -> arg@0:0:0:0: String suchMeta=false
        |    otherArg -> [byName]otherArg@1:0:1:1: Int suchMeta=false
        |    varargsy -> [repeated]varargsy@2:0:2:2: Seq[Double] suchMeta=false
        |  overload -> def overload: void
        |    AJDI: [hasDefaultValue]int@0:0:0:0: Int suchMeta=false
        |    ARGS:
        |    int -> [hasDefaultValue]int@0:0:0:0: Int suchMeta=false
        |  FUNCTIONS:
        |  varargsMethod -> def varargsMethod: Unit
        |    RENAMED:
        |    nejm -> [repeated]ints<nejm>@3:1:1:0: Seq[Int] suchMeta=false
        |    ARGS:
        |    krap -> krap@0:0:0:0: String suchMeta=false
        |    dubl -> dubl@1:0:1:1: Double suchMeta=false
        |    czy -> czy@2:1:0:2: Boolean suchMeta=false
        |  defaultValueMethod -> def defaultValueMethod: Unit
        |    RENAMED:
        |
        |    ARGS:
        |    int -> [hasDefaultValue]int@0:0:0:0: Int suchMeta=false
        |    bul -> bul@1:0:1:1: Boolean suchMeta=false
        |  generyk -> def generyk[T]: List[T]
        |    RENAMED:
        |
        |    ARGS:
        |    lel -> lel@0:0:0:0: com.avsystem.commons.rpc.Box[T] suchMeta=false @annotTypeString[com.avsystem.commons.rpc.Box[T]]
        |    tag -> [implicit]tag@1:1:0:1: com.avsystem.commons.rpc.Tag[T] suchMeta=false
        |  POSTERS:
        |  POST_postit -> POST() def postit<POST_postit>: String
        |    HEADERS:
        |    bar<X-Bar>@1:0:1:0: String suchMeta=false
        |    foo<X-Foo>@3:0:3:1: String suchMeta=true @suchMeta(3,c) @suchMeta(2,b)
        |    BODY:
        |    arg -> arg@0:0:0:0: String suchMeta=false
        |    int -> int@2:0:2:1: Int suchMeta=false
        |  GETTERS:
        |  ovgetter -> def overload<ovgetter>: com.avsystem.commons.rpc.TestApi
        |    ARGS:
        |    lel@0:0:0:0: String suchMeta=false
        |    RESULT: <recursive>
        |
        |  getit -> def getit: com.avsystem.commons.rpc.TestApi
        |    ARGS:
        |    stuff@0:0:0:0: String suchMeta=false
        |    otherStuff@1:0:1:0: List[Int] suchMeta=true @suchMeta(1,a)
        |    RESULT: <recursive>
        |  PREFIXERS:
        |  ovprefix -> def overload<ovprefix>: com.avsystem.commons.rpc.TestApi
        |    RESULT: <recursive>
        |""".stripMargin
    )
  }

  test("TestApi partial metadata") {
    assert(TestApi.partialMetadata.repr == "postit(X-Bar,X-Foo)")
  }
}
