package com.avsystem.commons
package rpc

import com.avsystem.commons.concurrent.{HasExecutionContext, RunNowEC}
import com.avsystem.commons.rpc.DummyRPC._
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}

import scala.collection.mutable.ArrayBuffer

class RPCTest extends WordSpec with Matchers with BeforeAndAfterAll {

  trait RunNowFutureCallbacks extends HasExecutionContext {
    protected implicit final def executionContext: ExecutionContext = RunNowEC
  }

  implicit class jsInterpolation(sc: StringContext) {
    def js(): String = write(sc.parts.mkString)
  }

  def get[T](f: Future[T]): T =
    f.value.get.get

  "rpc caller" should {
    "should properly deserialize RPC calls" in {
      val invocations = new ArrayBuffer[RawInvocation]
      val rawRpc = AsRawRPC[TestRPC].asRaw(TestRPC.rpcImpl((inv, _) => {
        invocations += inv
        inv.rpcName
      }))

      rawRpc.fire(RawInvocation("handleMore", Nil))
      rawRpc.fire(RawInvocation("doStuff", List("42", js"omgsrsly", "true")))
      assert(js"doStuffResult" == get(rawRpc.call(RawInvocation("doStuffBoolean", List("true")))))
      rawRpc.fire(RawInvocation("doStuffInt", List("5")))
      rawRpc.fire(RawInvocation("doStuffInt", Nil))
      rawRpc.fire(RawInvocation("handleMore", Nil))
      rawRpc.fire(RawInvocation("handle", Nil))
      rawRpc.fire(RawInvocation("takeCC", Nil))
      rawRpc.fire(RawInvocation("srslyDude", Nil))
      rawRpc.get(RawInvocation("innerRpc", List(js"innerName"))).fire(RawInvocation("proc", Nil))
      assert(js"innerRpc.funcResult" == get(rawRpc.get(RawInvocation("innerRpc", List(js"innerName")))
        .call(RawInvocation("func", List("42")))))
      assert(js"generallyDoStuffResult" ==
        get(rawRpc.call(RawInvocation("generallyDoStuff", List(js"String", "[\"generallyDoStuffResult\"]")))))

      assert(invocations.toList == List(
        RawInvocation("handleMore", Nil),
        RawInvocation("doStuff", List("42", js"omgsrsly", "true")),
        RawInvocation("doStuffBoolean", List("true")),
        RawInvocation("doStuffInt", List("5")),
        RawInvocation("doStuffInt", List("42")),
        RawInvocation("handleMore", Nil),
        RawInvocation("handle", Nil),
        RawInvocation("takeCC", List("""{"i":-1,"fuu":"_"}""")),
        RawInvocation("srslyDude", Nil),
        RawInvocation("innerRpc", List(js"innerName")),
        RawInvocation("innerRpc.proc", Nil),
        RawInvocation("innerRpc", List(js"innerName")),
        RawInvocation("innerRpc.func", List("42")),
        RawInvocation("generallyDoStuff", List(js"String", "[\"generallyDoStuffResult\"]"))
      ))
    }

    "fail on bad input" in {
      val rawRpc = AsRawRPC[TestRPC].asRaw(TestRPC.rpcImpl((_, _) => ()))
      intercept[UnknownRpc](rawRpc.fire(RawInvocation("whatever", Nil)))
      intercept[MissingRpcArgument](rawRpc.call(RawInvocation("doStuffBoolean", Nil)))
      intercept[InvalidRpcArgument](rawRpc.call(RawInvocation("doStuffBoolean", List("notbool"))))
    }

    "real rpc should properly serialize calls to raw rpc" in {
      val invocations = new ArrayBuffer[RawInvocation]

      object rawRpc extends RawRPC with RunNowFutureCallbacks {
        def fire(inv: RawInvocation): Unit =
          invocations += inv

        def call(inv: RawInvocation): Future[String] = {
          invocations += inv
          Future.successful(write(inv.rpcName + "Result"))
        }

        def get(inv: RawInvocation): RawRPC = {
          invocations += inv
          this
        }
      }

      val realRpc = AsRealRPC[TestRPC].asReal(rawRpc)

      realRpc.handleMore()
      realRpc.doStuff(42, "omgsrsly")(Some(true))
      assert("doStuffBooleanResult" == get(realRpc.doStuff(true)))
      realRpc.doStuff(5)
      realRpc.handleMore()
      realRpc.handle
      realRpc.innerRpc("innerName").proc()
      realRpc.innerRpc("innerName").moreInner("moreInner").moreInner("evenMoreInner").func(42)
      assert(get(realRpc.generallyDoStuff(List("generallyDoStuffResult"))).contains("generallyDoStuffResult"))

      assert(invocations.toList == List(
        RawInvocation("handleMore", Nil),
        RawInvocation("doStuff", List("42", js"omgsrsly", "true")),
        RawInvocation("doStuffBoolean", List("bul:true")),
        RawInvocation("doStuffInt", List("5")),
        RawInvocation("handleMore", Nil),
        RawInvocation("handle", Nil),

        RawInvocation("innerRpc", List(js"innerName")),
        RawInvocation("proc", Nil),

        RawInvocation("innerRpc", List(js"innerName")),
        RawInvocation("moreInner", List(js"moreInner")),
        RawInvocation("moreInner", List(js"evenMoreInner")),
        RawInvocation("func", List("42")),
        RawInvocation("generallyDoStuff", List(js"String", "[\"generallyDoStuffResult\"]"))
      ))
    }
  }

  trait BaseRPC[T] {
    def accept(t: T): Unit
  }

  trait ConcreteRPC extends BaseRPC[String]
  object ConcreteRPC extends RPCCompanion[ConcreteRPC]

  trait EmptyRPC
  object EmptyRPC extends RPCCompanion[EmptyRPC]
}
