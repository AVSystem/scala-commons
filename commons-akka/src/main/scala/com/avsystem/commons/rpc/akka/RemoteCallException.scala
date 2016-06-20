package com.avsystem.commons
package rpc.akka

/**
  * @author Wojciech Milewski
  */
final class RemoteCallException(exceptionName: String, remoteMessage: String) extends RuntimeException(
  s"""Exception on the server: $exceptionName.
      |Original message: $remoteMessage
      |Please check logs on the server for whole stacktrace.
   """.stripMargin)

object RemoteTimeoutException extends RuntimeException