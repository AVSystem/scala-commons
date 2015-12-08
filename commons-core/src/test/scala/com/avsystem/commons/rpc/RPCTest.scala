package com.avsystem.commons
package rpc

import com.avsystem.commons.concurrent.{HasExecutionContext, RunNowEC}
import com.github.ghik.silencer.silent
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}
import upickle.Js

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{ExecutionContext, Future}

class RPCTest extends WordSpec with Matchers with BeforeAndAfterAll {

  trait RunNowFutureCallbacks extends HasExecutionContext {
    protected implicit final def executionContext: ExecutionContext = RunNowEC
  }

  def get[T](f: Future[T]) =
    f.value.get.get

  "rpc caller" should {
    "should properly deserialize RPC calls" in {
      val invocations = new ArrayBuffer[(String, List[List[Any]])]
      val rawRpc = AsRawRPC[TestRPC].asRaw(TestRPC.rpcImpl((name, args, _) => {
        invocations += ((name, args))
        name
      }))

      rawRpc.fire("handleMore", List(Nil))
      rawRpc.fire("doStuff", List(List(Js.Num(42), Js.Str("omgsrsly")), List(Js.Arr(Js.True))))
      assert(Js.Str("doStuffResult") === get(rawRpc.call("doStuff", List(List(Js.True)))))
      rawRpc.fire("doStuffInt", List(List(Js.Num(5))))
      rawRpc.fire("handleMore", List(Nil))
      rawRpc.fire("handle", Nil)
      rawRpc.fire("srslyDude", List(Nil))
      rawRpc.get("innerRpc", List(List(Js.Str("innerName")))).fire("proc", List(Nil))
      assert(Js.Str("innerRpc.funcResult") === get(rawRpc.get("innerRpc", List(List(Js.Str("innerName")))).call("func", List(List(Js.Num(42))))))

      assert(invocations.toList === List(
        ("handleMore", List(Nil)),
        ("doStuff", List(List(42, "omgsrsly"), List(Some(true)))),
        ("doStuff", List(List(true))),
        ("doStuffInt", List(List(5))),
        ("handleMore", List(Nil)),
        ("handle", Nil),
        ("srslyDude", List(Nil)),
        ("innerRpc", List(List("innerName"))),
        ("innerRpc.proc", List(Nil)),
        ("innerRpc", List(List("innerName"))),
        ("innerRpc.func", List(List(42)))
      ))
    }

    "fail on bad input" in {
      val rawRpc = AsRawRPC[TestRPC].asRaw(TestRPC.rpcImpl((_, _, _) => ()))
      intercept[Exception](rawRpc.fire("whatever", Nil))
    }

    "real rpc should properly serialize calls to raw rpc" in {
      val invocations = new ArrayBuffer[(String, List[List[Any]])]

      object rawRpc extends RawRPC with RunNowFutureCallbacks {
        def fire(rpcName: String, args: List[List[Js.Value]]): Unit =
          invocations += ((rpcName, args))

        def call(rpcName: String, args: List[List[Js.Value]]): Future[Js.Value] = {
          invocations += ((rpcName, args))
          Future.successful(Js.Str(rpcName + "Result"))
        }

        def get(rpcName: String, args: List[List[Js.Value]]): RawRPC = {
          invocations += ((rpcName, args))
          this
        }
      }

      @silent
      val realRpc = AsRealRPC[TestRPC].asReal(rawRpc)

      realRpc.handleMore()
      realRpc.doStuff(42, "omgsrsly")(Some(true))
      assert("doStuffResult" === get(realRpc.doStuff(true)))
      realRpc.doStuff(5)
      realRpc.handleMore()
      realRpc.handle
      realRpc.innerRpc("innerName").proc()
      realRpc.innerRpc("innerName").func(42)

      assert(invocations.toList === List(
        ("handleMore", List(Nil)),
        ("doStuff", List(List(Js.Num(42), Js.Str("omgsrsly")), List(Js.Arr(Js.True)))),
        ("doStuff", List(List(Js.True))),
        ("doStuffInt", List(List(Js.Num(5)))),
        ("handleMore", List(Nil)),
        ("handle", Nil),
        ("innerRpc", List(List(Js.Str("innerName")))),
        ("proc", List(Nil)),
        ("innerRpc", List(List(Js.Str("innerName")))),
        ("func", List(List(Js.Num(42))))
      ))
    }

    "rpc should work with parameterized interface types" in {
      @RPC trait BaseRPC[T] {
        def accept(t: T): Unit
      }

      trait ConcreteRPC extends BaseRPC[String]

      AsRawRPC[ConcreteRPC]
      AsRealRPC[ConcreteRPC]
    }

    "rpc should work with empty interface types" in {
      @RPC trait EmptyRPC

      AsRawRPC[EmptyRPC]: @silent
      AsRealRPC[EmptyRPC]: @silent
    }
  }
}
