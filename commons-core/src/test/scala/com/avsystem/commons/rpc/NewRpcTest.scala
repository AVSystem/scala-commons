package com.avsystem.commons
package rpc

import com.avsystem.commons.serialization.whenAbsent
import com.github.ghik.silencer.silent
import org.scalatest.FunSuite

trait SomeBase {
  def difolt: Boolean = true

  @POST def postit(arg: String, @header("X-Bar") bar: String, int: Int, @header("X-Foo") foo: String): String
}

trait TestApi extends SomeBase {
  def varargsMethod(krap: String, dubl: Double)(czy: Boolean, @renamed(42, "nejm") ints: Int*): Future[Unit]
  def defaultValueMethod(int: Int = 0, @whenAbsent(difolt) bul: Boolean): Future[Unit]
  def flames(arg: String, otherArg: => Int, varargsy: Double*): Unit
  def overload(int: Int): Unit
  def overload: TestApi
  def getit(stuff: String, otherStuff: List[Int]): TestApi
  def postit(arg: String, bar: String, int: Int, foo: String): String
}
object TestApi {
  implicit val asRealRaw: NewRawRpc.AsRealRawRpc[TestApi] = NewRawRpc.materializeAsRealRaw[TestApi]
  implicit val metadata: NewRpcMetadata[TestApi] = NewRpcMetadata.materializeForRpc[TestApi]
}

object NewRpcTest {
  implicit val innerRpcAsRealRaw: NewRawRpc.AsRealRawRpc[InnerRPC] = NewRawRpc.materializeAsRealRaw[InnerRPC]
  implicit val innerRpcMetadata: NewRpcMetadata[InnerRPC] = NewRpcMetadata.materializeForRpc[InnerRPC]
  @silent
  implicit val testRpcAsRealRaw: NewRawRpc.AsRealRawRpc[TestRPC] = NewRawRpc.materializeAsRealRaw[TestRPC]
  implicit val testRpcMetadata: NewRpcMetadata[TestRPC] = NewRpcMetadata.materializeForRpc[TestRPC]
}

class MetadataTest extends FunSuite {
  test("TestApi metadata") {
    assert(TestApi.metadata.toString ==
      """TestApi
        |  PROCEDURES:
        |  overload -> def overload: void
        |    AJDI: int@0:0:0:0: int
        |    ARGS:
        |    int -> int@0:0:0:0: int
        |  flames -> def flames: void
        |    NO AJDI
        |    ARGS:
        |    arg -> arg@0:0:0:0: String
        |    otherArg -> [byName]otherArg@1:0:1:1: int
        |    varargsy -> [repeated]varargsy@2:0:2:2: Seq
        |  FUNCTIONS:
        |  defaultValueMethod -> def defaultValueMethod: void
        |    RENAMED:
        |
        |    ARGS:
        |    int -> [hasDefaultValue]int@0:0:0:0: int
        |    bul -> bul@1:0:1:1: boolean
        |  varargsMethod -> def varargsMethod: void
        |    RENAMED:
        |    nejm -> [repeated]ints<nejm>@3:1:1:0: Seq renames=renamed(42,nejm)
        |    ARGS:
        |    krap -> krap@0:0:0:0: String
        |    dubl -> dubl@1:0:1:1: double
        |    czy -> czy@2:1:0:2: boolean
        |  POSTERS:
        |  postit -> POST() def postit: String
        |    HEADERS:
        |    bar<X-Bar>@1:0:1:0: String
        |    foo<X-Foo>@3:0:3:1: String
        |    BODY:
        |    arg -> arg@0:0:0:0: String
        |    int -> int@2:0:2:1: int
        |  GETTERS:
        |  getit -> def getit: TestApi
        |    ARGS:
        |    stuff@0:0:0:0: String
        |    otherStuff@1:0:1:1: List
        |    RESULT: <recursive>
        |  overload -> def overload: TestApi
        |    ARGS:
        |
        |    RESULT: <recursive>""".stripMargin
    )
  }

  test("TestRPC metadata") {
    assert(NewRpcTest.testRpcMetadata.toString ==
      """TestRPC
        |  PROCEDURES:
        |  handleMore -> def handleMore: void
        |    NO AJDI
        |    ARGS:
        |
        |  takeCC -> def takeCC: void
        |    NO AJDI
        |    ARGS:
        |    r -> [hasDefaultValue]r@0:0:0:0: Record
        |  srslyDude -> def srslyDude: void
        |    NO AJDI
        |    ARGS:
        |
        |  doStuff -> def doStuff: void
        |    AJDI: lol@0:0:0:0: int
        |    ARGS:
        |    lol -> lol@0:0:0:0: int
        |    fuu -> [hasDefaultValue]fuu@1:0:1:1: String
        |    cos -> [implicit]cos@2:1:0:2: Option
        |  doStuffInt -> def doStuff<doStuffInt>: void
        |    AJDI: num@0:0:0:0: int
        |    ARGS:
        |    num -> num@0:0:0:0: int
        |  handle -> def handle: void
        |    NO AJDI
        |    ARGS:
        |
        |  FUNCTIONS:
        |  doStuffBoolean -> def doStuff<doStuffBoolean>: String
        |    RENAMED:
        |
        |    ARGS:
        |    yes -> yes@0:0:0:0: boolean
        |  POSTERS:
        |
        |  GETTERS:
        |  innerRpc -> def innerRpc: InnerRPC
        |    ARGS:
        |    name@0:0:0:0: String
        |    RESULT: InnerRPC
        |      PROCEDURES:
        |      proc -> def proc: void
        |        NO AJDI
        |        ARGS:
        |
        |      FUNCTIONS:
        |      func -> def func: String
        |        RENAMED:
        |
        |        ARGS:
        |        arg -> arg@0:0:0:0: int
        |      POSTERS:
        |
        |      GETTERS:
        |      indirectRecursion -> def indirectRecursion: TestRPC
        |        ARGS:
        |
        |        RESULT: <recursive>
        |      moreInner -> def moreInner: InnerRPC
        |        ARGS:
        |        name@0:0:0:0: String
        |        RESULT: <recursive>""".stripMargin)
  }
}
