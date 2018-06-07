package com.avsystem.commons
package rpc

import com.avsystem.commons.serialization.whenAbsent
import org.scalatest.FunSuite

trait SomeBase {
  def difolt: Boolean = true

  @POST def postit(arg: String, @header("X-Bar") bar: String, int: Int, @header("X-Foo") @suchMeta(2, "b") foo: String): String
}

trait TestApi extends SomeBase {
  def doSomething(double: Double): String
  def doSomethingElse(double: Double): String
  def varargsMethod(krap: String, dubl: Double)(czy: Boolean, @renamed(42, "nejm") ints: Int*): Future[Unit]
  def defaultValueMethod(int: Int = 0, @whenAbsent(difolt) bul: Boolean): Future[Unit]
  def flames(arg: String, otherArg: => Int, varargsy: Double*): Unit
  def overload(int: Int): Unit
  def overload(lel: String): TestApi
  def overload: TestApi
  def getit(stuff: String, @suchMeta(1, "a") otherStuff: List[Int]): TestApi
  def postit(arg: String, bar: String, int: Int, @suchMeta(3, "c") foo: String): String
}
object TestApi {
  implicit val AsRawReal: NewRawRpc.AsRawRealRpc[TestApi] = NewRawRpc.materializeAsRawReal[TestApi]
  implicit val metadata: NewRpcMetadata[TestApi] = NewRpcMetadata.materializeForRpc[TestApi]
}

class NewRpcMetadataTest extends FunSuite {
  test("TestApi metadata") {
    assert(TestApi.metadata.toString ==
      """TestApi
        |  DO SOMETHING ELSE: true
        |  PROCEDURES:
        |  overload -> def overload: void
        |    AJDI: int@0:0:0:0: int suchMeta=false
        |    ARGS:
        |    int -> int@0:0:0:0: int suchMeta=false
        |  flames -> def flames: void
        |    NO AJDI
        |    ARGS:
        |    arg -> arg@0:0:0:0: String suchMeta=false
        |    otherArg -> [byName]otherArg@1:0:1:1: int suchMeta=false
        |    varargsy -> [repeated]varargsy@2:0:2:2: Seq suchMeta=false
        |  FUNCTIONS:
        |  defaultValueMethod -> def defaultValueMethod: void
        |    RENAMED:
        |
        |    ARGS:
        |    int -> [hasDefaultValue]int@0:0:0:0: int suchMeta=false
        |    bul -> bul@1:0:1:1: boolean suchMeta=false
        |  varargsMethod -> def varargsMethod: void
        |    RENAMED:
        |    nejm -> [repeated]ints<nejm>@3:1:1:0: Seq suchMeta=false
        |    ARGS:
        |    krap -> krap@0:0:0:0: String suchMeta=false
        |    dubl -> dubl@1:0:1:1: double suchMeta=false
        |    czy -> czy@2:1:0:2: boolean suchMeta=false
        |  POSTERS:
        |  postit -> POST() def postit: String
        |    HEADERS:
        |    bar<X-Bar>@1:0:1:0: String suchMeta=false
        |    foo<X-Foo>@3:0:3:1: String suchMeta=true,metas=suchMeta(3,c),suchMeta(2,b)
        |    BODY:
        |    arg -> arg@0:0:0:0: String suchMeta=false
        |    int -> int@2:0:2:1: int suchMeta=false
        |  GETTERS:
        |  getit -> def getit: TestApi
        |    ARGS:
        |    stuff@0:0:0:0: String suchMeta=false
        |    otherStuff@1:0:1:0: List suchMeta=true,metas=suchMeta(1,a)
        |    RESULT: <recursive>
        |
        |  overload -> def overload: TestApi
        |    ARGS:
        |    lel@0:0:0:0: String suchMeta=false
        |    RESULT: <recursive>
        |  PREFIXERS:
        |  overload -> def overload: TestApi
        |    RESULT: <recursive>
        |""".stripMargin
    )
  }
}
