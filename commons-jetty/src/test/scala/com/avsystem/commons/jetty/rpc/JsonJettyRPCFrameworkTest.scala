package com.avsystem.commons
package jetty.rpc

import com.avsystem.commons.rpc.RPC
import org.eclipse.jetty.client.HttpClient
import org.eclipse.jetty.server.Server
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Milliseconds, Seconds, Span}
import org.scalatest.{BeforeAndAfterAll, FunSuite, Matchers}

/**
  * @author MKej
  */
class JsonJettyRPCFrameworkTest extends FunSuite with ScalaFutures with Matchers with BeforeAndAfterAll {
  import JsonJettyRPCFramework._

  import scala.concurrent.ExecutionContext.Implicits.global

  override implicit def patienceConfig: PatienceConfig =
    PatienceConfig(scaled(Span(5, Seconds)), scaled(Span(50, Milliseconds)))

  @RPC trait SomeApi {
    def keks: Future[Long]
    def isTop(keks: Long): Future[Boolean]
    def topper: Topper
    def erroneousKeks: Future[Int]
  }
  object SomeApi {
    implicit val fullRPCInfo: BaseFullRPCInfo[SomeApi] = materializeFullInfo
  }

  @RPC trait Topper {
    def initialize: Future[Unit]
    def initialize2(): Future[Unit]
    def topKeks: Future[Int]
    def hello(world: String): Future[String]
    def hellos(world1: String, world2: Int): Future[String]
    def currys(curry1: String)(curry2: Int = 30): Future[String]
  }
  object Topper {
    implicit val fullRPCInfo: BaseFullRPCInfo[Topper] = materializeFullInfo
  }

  val keksResult = Long.MaxValue
  val topKeksResult = Int.MaxValue
  val errorMessage = "cannot into"

  val impl = new SomeApi {
    def keks = Future.successful(keksResult)
    def isTop(keks: Long) = Future.successful(keks == Int.MaxValue)
    object topper extends Topper {
      def initialize = Future.successful(println("Topper initialized"))
      def initialize2() = initialize
      def topKeks = Future.successful(topKeksResult)
      def hello(world: String) = Future.successful(world)
      def hellos(world1: String, world2: Int) = Future.successful(world1 + world2)
      def currys(curry1: String)(curry2: Int) = Future.successful(curry1 + curry2)
    }
    def erroneousKeks: Future[Int] = Future.failed(new RuntimeException(errorMessage))
  }

  val port = 1337
  val server = new Server(port).setup(_.setHandler(JsonJettyRPCFramework.newHandler[SomeApi](impl)))
  val httpClient = new HttpClient()
  val rpc = JsonJettyRPCFramework.newClient[SomeApi](httpClient, s"http://localhost:${1337}/")

  test("empty-paren -> unit") {
    noException should be thrownBy rpc.topper.initialize.futureValue
  }

  test("paren -> unit") {
    noException should be thrownBy rpc.topper.initialize2().futureValue
    noException should be thrownBy rpc.topper.initialize2.futureValue
  }

  test("empty-paren -> long") {
    rpc.keks.futureValue shouldBe keksResult
  }

  test("single arg -> boolean") {
    rpc.isTop(keksResult).futureValue shouldBe false
    rpc.isTop(topKeksResult).futureValue shouldBe true
  }

  test("inner rpc + empty-paren -> int") {
    rpc.topper.topKeks.futureValue shouldBe topKeksResult
  }

  test("inner rpc + string arg -> string") {
    val world = "world"
    rpc.topper.hello(world).futureValue shouldBe world
    val anonymous = ""
    rpc.topper.hello(anonymous).futureValue shouldBe anonymous
  }

  test("inner rpc + multi arg -> string") {
    rpc.topper.hellos("world", 42).futureValue shouldBe "world42"
  }

  test("multiple argument lists") {
    rpc.topper.currys("world")(42).futureValue shouldBe "world42"
  }

  test("multiple argument lists + default value") {
    rpc.topper.currys("world")().futureValue shouldBe "world30"
  }

  test("empty-paren -> error msg") {
    val failed = rpc.erroneousKeks.failed.futureValue
    failed shouldBe a[HttpException]
    val exception = failed.asInstanceOf[HttpException]
    exception.reason shouldBe errorMessage
    exception.status shouldBe 500
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    server.start()
    httpClient.start()
  }

  override protected def afterAll(): Unit = {
    server.stop()
    httpClient.stop()
    super.afterAll()
  }
}
