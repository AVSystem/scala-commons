package com.avsystem.commons
package jetty.rpc

import org.eclipse.jetty.server.Server

object RestHandlerMain {
  def main(args: Array[String]): Unit = {
    val handler = new RestHandler(SomeApi.asHandleRequest(SomeApi.impl))
    val server = new Server(9090)
    server.setHandler(handler)
    server.start()
    server.join()
  }
}
