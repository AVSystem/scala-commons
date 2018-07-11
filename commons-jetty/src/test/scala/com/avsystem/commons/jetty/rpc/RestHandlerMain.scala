package com.avsystem.commons
package jetty.rpc

import com.avsystem.commons.jetty.rest.RestHandler
import org.eclipse.jetty.server.Server

object RestHandlerMain {
  def main(args: Array[String]): Unit = {
    val handler = RestHandler[SomeApi](SomeApi.impl)
    val server = new Server(9090)
    server.setHandler(handler)
    server.start()
    server.join()
  }
}
