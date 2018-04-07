package com.avsystem.commons
package rpc

import com.avsystem.commons.serialization.HasGenCodec
import com.github.ghik.silencer.silent


case class Record(i: Int, fuu: String)
object Record extends HasGenCodec[Record]

@RPC trait InnerRPC {
  def proc(): Unit

  def func(arg: Int): Future[String]

  def moreInner(name: String): InnerRPC

  def indirectRecursion(): TestRPC
}
object InnerRPC extends DummyRPC.RPCCompanion[InnerRPC]

@RPC trait TestRPC {
  @silent
  def handle: Unit

  def handleMore(): Unit

  def doStuff(lol: Int, fuu: String = "pisiont")(implicit cos: Option[Boolean]): Unit

  @RPCName("doStuffBoolean")
  def doStuff(yes: Boolean): Future[String]

  @RPCName("doStuffInt")
  def doStuff(num: Int): Unit

  def takeCC(r: Record): Unit

  def srslyDude(): Unit

  def innerRpc(name: String): InnerRPC
}

@silent
object TestRPC extends DummyRPC.RPCCompanion[TestRPC] {
  def rpcImpl(onInvocation: (String, List[List[Any]], Option[Any]) => Any) = new TestRPC { outer =>
    private def onProcedure(methodName: String, args: List[List[Any]]): Unit =
      onInvocation(methodName, args, None)

    private def onCall[T](methodName: String, args: List[List[Any]], result: T): Future[T] = {
      onInvocation(methodName, args, Some(result))
      Future.successful(result)
    }

    private def onGet[T](methodName: String, args: List[List[Any]], result: T): T = {
      onInvocation(methodName, args, None)
      result
    }

    def handleMore(): Unit =
      onProcedure("handleMore", List(Nil))

    def doStuff(lol: Int, fuu: String)(implicit cos: Option[Boolean]): Unit =
      onProcedure("doStuff", List(List(lol, fuu), List(cos)))

    def doStuff(yes: Boolean): Future[String] =
      onCall("doStuffBoolean", List(List(yes)), "doStuffResult")

    def doStuff(num: Int): Unit =
      onProcedure("doStuffInt", List(List(num)))

    def handle: Unit =
      onProcedure("handle", Nil)

    def takeCC(r: Record): Unit =
      onProcedure("recordCC", List(List(r)))

    def srslyDude(): Unit =
      onProcedure("srslyDude", List(Nil))

    def innerRpc(name: String): InnerRPC = {
      onInvocation("innerRpc", List(List(name)), None)
      new InnerRPC {
        def func(arg: Int): Future[String] =
          onCall("innerRpc.func", List(List(arg)), "innerRpc.funcResult")

        def proc(): Unit =
          onProcedure("innerRpc.proc", List(Nil))

        def moreInner(name: String) =
          this

        def indirectRecursion() =
          outer
      }
    }
  }
}
