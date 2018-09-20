package com.avsystem.commons
package okhttp.rest

import com.avsystem.commons.jetty.rest.RestServlet
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.servlet.{ServletHandler, ServletHolder}
import org.scalatest.FunSuite
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Seconds, Span}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps

class RestTest extends FunSuite with UsesJettyHttpServer with UsesOkHttpClient with ScalaFutures {

  override protected def setupServer(server: Server): Unit = {
    val servlet = RestServlet[SomeApi](new SomeApiImpl())
    val holder = new ServletHolder(servlet)
    val handler = new ServletHandler
    handler.addServletWithMapping(holder, "/*")
    server.setHandler(handler)
  }

  test("GET method") {
    val proxy = RestClient[SomeApi](client, scheme, host, port)
    val result = proxy.hello("Odersky")
    whenReady(result, Timeout(Span(5, Seconds))) { s => assert(s === "Hello, Odersky!")}
  }

  test("POST method") {
    val proxy = RestClient[SomeApi](client, scheme, host, port)
    val result = proxy.helloThere("Martin")
    whenReady(result, Timeout(Span(5, Seconds))) { s => assert(s === "Hello, Martin!")}

  }

  test("Headers test") {
    val proxy = RestClient[SomeApi](client, scheme, host, port)
    val result = proxy.helloWithHeader("Martin O")
    whenReady(result, Timeout(Span(5, Seconds))) { s => assert(s === "Hello, Martin O!")}
  }

  test("Query param test") {
    val proxy = RestClient[SomeApi](client, scheme, host, port)
    val result = proxy.helloWithQueryParam("Odersky Martin")
    whenReady(result, Timeout(Span(5, Seconds))) { s => assert(s === "Hello, Odersky Martin!")}
  }

  test("Path Param test") {
    val proxy = RestClient[SomeApi](client, scheme, host, port)
    val result = proxy.helloWithPathParam("Mr Martin")
    whenReady(result, Timeout(Span(5, Seconds))) { s => assert(s === "Hello, Mr Martin!")}
  }

  test("Poison test") {
    val proxy = RestClient[SomeApi](client, scheme, host, port)
    val result = proxy.helloWithPathParam(SomeApi.poison)
    Await.ready(result, 10 seconds).value.get match {
      case Failure(e) => assert(e.getMessage === "Server Error")
      case Success(s) => throw new Exception("poison should return server failure")
    }
  }
}