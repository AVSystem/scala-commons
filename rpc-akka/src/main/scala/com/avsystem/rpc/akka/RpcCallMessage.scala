package com.avsystem.rpc.akka

/**
  * @author Wojciech Milewski
  */
case class RpcCallMessage(name: String, paramLists: List[List[Any]])
