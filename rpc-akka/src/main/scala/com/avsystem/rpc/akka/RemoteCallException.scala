package com.avsystem.rpc.akka

/**
  * @author Wojciech Milewski
  */
class RemoteCallException(exceptionName: String) extends RuntimeException(s"Remote call exception: $exceptionName")
