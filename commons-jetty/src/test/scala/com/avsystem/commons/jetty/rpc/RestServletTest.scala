package com.avsystem.commons
package jetty.rpc

import java.nio.charset.StandardCharsets

import com.avsystem.commons.jetty.rest.RestServlet
import org.eclipse.jetty.client.util.StringContentProvider
import org.eclipse.jetty.http.{HttpMethod, HttpStatus}
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.{ServletHandler, ServletHolder}
import org.scalatest.FunSuite

class RestServletTest extends FunSuite with UsesHttpServer with UsesHttpClient {
  override protected def setupServer(server: Server): Unit = {
    val servlet = RestServlet[SomeApi](SomeApi.impl)
    val holder = new ServletHolder(servlet)
    val handler = new ServletHandler
    handler.addServletWithMapping(holder, "/*")
    server.setHandler(handler)
  }

  test("GET method") {
    val response = client.newRequest(s"$baseUrl/hello")
      .method(HttpMethod.GET)
      .param("who", "World")
      .send()

    assert(response.getContentAsString === """"Hello, World!"""")
  }

  test("POST method") {
    val response = client.newRequest(s"$baseUrl/hello")
      .method(HttpMethod.POST)
      .content(new StringContentProvider("application/json", """{"who":"World"}""", StandardCharsets.UTF_8))
      .send()

    assert(response.getContentAsString === """"Hello, World!"""")
  }

  test("error handling") {
    val response = client.newRequest(s"$baseUrl/hello")
      .method(HttpMethod.GET)
      .param("who", SomeApi.poison)
      .send()

    assert(response.getStatus === HttpStatus.INTERNAL_SERVER_ERROR_500)
    assert(response.getContentAsString === SomeApi.poison)
  }

  test("invalid path") {
    val response = client.newRequest(s"$baseUrl/invalidPath")
      .method(HttpMethod.GET)
      .send()

    assert(response.getStatus === HttpStatus.NOT_FOUND_404)
  }
}
