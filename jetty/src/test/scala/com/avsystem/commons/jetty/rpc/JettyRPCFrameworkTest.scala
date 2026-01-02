package com.avsystem.commons
package jetty.rpc

import org.eclipse.jetty.client.HttpClient
import org.eclipse.jetty.server.Server
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time.{Milliseconds, Seconds, Span}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** @author
  *   MKej
  */
class JettyRPCFrameworkTest extends AnyFunSuite with ScalaFutures with Matchers with BeforeAndAfterAll {

  import JettyRPCFramework._

  override implicit def patienceConfig: PatienceConfig =
    PatienceConfig(scaled(Span(5, Seconds)), scaled(Span(50, Milliseconds)))

  trait SomeApi {
    def keks: Future[Long]
    def isTop(keks: Long): Future[Boolean]
    def topper: Topper
    def differentTopper(helloPattern: String): Topper
    def erroneousKeks: Future[Int]
  }
  object SomeApi extends RPCCompanion[SomeApi]

  trait Topper {
    def initialize: Future[Unit]
    def initialize2(): Future[Unit]
    def topKeks: Future[Int]
    def hello(world: String): Future[String]
    def hellos(world1: String, world2: Int): Future[String]
    def currys(curry1: String)(curry2: Int = 30): Future[String]
  }
  object Topper extends RPCCompanion[Topper]

  class TopperImpl(helloPattern: String, topKeksResult: Int) extends Topper {
    override def initialize: Future[Unit] = Future.eval(println("Topper initialized"))
    override def initialize2(): Future[Unit] = initialize
    override def topKeks: Future[Int] = Future.successful(topKeksResult)
    override def hello(world: String): Future[String] = Future.eval(helloPattern.format(world))
    override def hellos(world1: String, world2: Int): Future[String] = Future.successful(world1 + world2)
    override def currys(curry1: String)(curry2: Int): Future[String] = Future.successful(curry1 + curry2)
  }

  val keksResult: Long = Long.MaxValue
  val topKeksResult: Int = Int.MaxValue

  val impl: SomeApi = new SomeApi {
    override def keks: Future[Long] = Future.successful(keksResult)
    override def isTop(keks: Long): Future[Boolean] = Future.successful(keks == Int.MaxValue)
    override val topper: TopperImpl = new TopperImpl("%s", topKeksResult)
    override def differentTopper(helloPattern: String): Topper = new TopperImpl(helloPattern, topKeksResult)
    override def erroneousKeks: Future[Int] = Future.failed(new RuntimeException("cannot into"))
  }

  val port: Int = 1337
  val server: Server = new Server(port).setup(_.setHandler(JettyRPCFramework.newHandler[SomeApi](impl)))
  val httpClient: HttpClient = new HttpClient()
  val rpc: SomeApi = JettyRPCFramework.newClient[SomeApi](httpClient, s"http://localhost:${1337}/")

  test("empty-paren -> unit") {
    noException should be thrownBy rpc.topper.initialize.futureValue
  }

  test("paren -> unit") {
    noException should be thrownBy rpc.topper.initialize2().futureValue
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

  test("inner rpc with args + string arg -> string") {
    val helloPattern = "Hello, %s!"
    val world = "world"
    rpc.differentTopper(helloPattern).hello(world).futureValue shouldBe helloPattern.format(world)
    val anonymous = ""
    rpc.differentTopper(helloPattern).hello(anonymous).futureValue shouldBe helloPattern.format(anonymous)
  }

  test("inner rpc + string arg -> string") {
    val world = "gżegżółka"
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
