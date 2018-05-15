package com.avsystem.commons
package rpc

import com.avsystem.commons.concurrent.{HasExecutionContext, RunNowEC}
import com.avsystem.commons.rpc.DummyRPC._
import com.github.ghik.silencer.silent
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}

import scala.collection.mutable.ArrayBuffer

class RPCTest extends WordSpec with Matchers with BeforeAndAfterAll {

  trait RunNowFutureCallbacks extends HasExecutionContext {
    protected implicit final def executionContext: ExecutionContext = RunNowEC
  }

  def get[T](f: Future[T]) =
    f.value.get.get

  "rpc caller" should {
    "should properly deserialize RPC calls" in {
      val invocations = new ArrayBuffer[(String, List[Any])]
      val rawRpc = AsRawRPC[TestRPC].asRaw(TestRPC.rpcImpl((name, args, _) => {
        invocations += ((name, args))
        name
      }))

      rawRpc.fire("handleMore", Nil)
      rawRpc.fire("doStuff", List(42, "omgsrsly", Some(true)))
      assert("doStuffResult" === get(rawRpc.call("doStuffBoolean", List(true))))
      rawRpc.fire("doStuffInt", List(5))
      rawRpc.fire("handleMore", Nil)
      rawRpc.fire("handle", Nil)
      rawRpc.fire("srslyDude", Nil)
      rawRpc.get("innerRpc", List("innerName")).fire("proc", Nil)
      assert("innerRpc.funcResult" === get(rawRpc.get("innerRpc", List("innerName")).call("func", List(42))))

      assert(invocations.toList === List(
        ("handleMore", Nil),
        ("doStuff", List(42, "omgsrsly", Some(true))),
        ("doStuffBoolean", List(true)),
        ("doStuffInt", List(5)),
        ("handleMore", Nil),
        ("handle", Nil),
        ("srslyDude", Nil),
        ("innerRpc", List("innerName")),
        ("innerRpc.proc", Nil),
        ("innerRpc", List("innerName")),
        ("innerRpc.func", List(42))
      ))
    }

    "fail on bad input" in {
      val rawRpc = AsRawRPC[TestRPC].asRaw(TestRPC.rpcImpl((_, _, _) => ()))
      intercept[Exception](rawRpc.fire("whatever", Nil))
    }

    "real rpc should properly serialize calls to raw rpc" in {
      val invocations = new ArrayBuffer[(String, List[Any])]

      object rawRpc extends RawRPC with RunNowFutureCallbacks {
        def fire(rpcName: String, args: List[Any]): Unit =
          invocations += ((rpcName, args))

        def call(rpcName: String, args: List[Any]): Future[Any] = {
          invocations += ((rpcName, args))
          Future.successful(rpcName + "Result")
        }

        def get(rpcName: String, args: List[Any]): RawRPC = {
          invocations += ((rpcName, args))
          this
        }
      }

      @silent
      val realRpc = AsRealRPC[TestRPC].asReal(rawRpc)

      realRpc.handleMore()
      realRpc.doStuff(42, "omgsrsly")(Some(true))
      assert("doStuffBooleanResult" === get(realRpc.doStuff(true)))
      realRpc.doStuff(5)
      realRpc.handleMore()
      realRpc.handle
      realRpc.innerRpc("innerName").proc()
      realRpc.innerRpc("innerName").moreInner("moreInner").moreInner("evenMoreInner").func(42)

      assert(invocations.toList === List(
        ("handleMore", Nil),
        ("doStuff", List(42, "omgsrsly", Some(true))),
        ("doStuffBoolean", List(true)),
        ("doStuffInt", List(5)),
        ("handleMore", Nil),
        ("handle", Nil),

        ("innerRpc", List("innerName")),
        ("proc", Nil),

        ("innerRpc", List("innerName")),
        ("moreInner", List("moreInner")),
        ("moreInner", List("evenMoreInner")),
        ("func", List(42))
      ))
    }

    trait BaseRPC[T] {
      def accept(t: T): Unit
    }

    trait ConcreteRPC extends BaseRPC[String]

    "rpc should work with parameterized interface types" in {
      materializeFullInfo[ConcreteRPC]
    }

    trait EmptyRPC

    "rpc should work with empty interface types" in {
      materializeFullInfo[EmptyRPC]: @silent
    }
  }
}
