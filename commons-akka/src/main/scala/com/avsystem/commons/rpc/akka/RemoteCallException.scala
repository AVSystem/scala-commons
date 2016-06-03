package com.avsystem.commons
package rpc.akka

/**
  * @author Wojciech Milewski
  */
class RemoteCallException(exceptionName: String) extends RuntimeException(
  s"""Exception on the server: $exceptionName.
     |Please check logs on the server for whole stacktrace.
   """.stripMargin)

object RemoteTimeoutException extends RuntimeException