package com.avsystem.commons
package rpc.akka

import akka.actor.{ActorPath, ActorSystem}
import akka.stream.ActorMaterializer
import monifu.concurrent.Implicits.globalScheduler
import monifu.reactive.Observable
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.Answer
import org.scalatest.concurrent.AsyncAssertions.Waiter
import org.scalatest.concurrent.{PatienceConfiguration, ScalaFutures}
import org.scalatest.mock.MockitoSugar
import org.scalatest.time.{Seconds, Span}
import org.scalatest.{BeforeAndAfterAll, FlatSpec, Matchers}

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

/**
  * @author Wojciech Milewski
  */
class AkkaRPCFrameworkTest extends FlatSpec with Matchers with MockitoSugar with BeforeAndAfterAll with ScalaFutures {

  import AkkaRPCFrameworkTest._

  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  case class Fixture(rpc: TestRPC, mockRpc: TestRPC, mockInnerRpc: InnerRPC)

  def fixture(testCode: Fixture => Any): Unit = {
    val testRpcMock = mock[TestRPC]
    val innerRpcMock = mock[InnerRPC]
    val serverActor = AkkaRPCFramework.serverActor[TestRPC](testRpcMock)
    val rpc = AkkaRPCFramework.client[TestRPC](serverActor.path)
    try {
      testCode(Fixture(rpc = rpc, mockRpc = testRpcMock, mockInnerRpc = innerRpcMock))
    } finally {
      system.stop(serverActor)
    }
  }

  def noConnectionFixture(testCode: Fixture => Any): Unit = {
    val mockRpc = mock[TestRPC]
    val mockInnerRpc = mock[InnerRPC]
    val rpc = AkkaRPCFramework.client[TestRPC](ActorPath.fromString("akka://user/thisactorshouldnotexists"))
    testCode(Fixture(rpc = rpc, mockRpc = mockRpc, mockInnerRpc = mockInnerRpc))
  }

  "Akka RPC Framework" should "successfully call unit method" in fixture { f =>
    val mockMethodCall = registerForUnitMethod(f.mockRpc.fireAndForget())

    f.rpc.fireAndForget()

    whenReady(mockMethodCall) { res =>
      verify(f.mockRpc).fireAndForget() //call to verify that method was called exactly once
    }
  }

  it should "does nothing when there is no connection to server actor and unit method called" in noConnectionFixture { f =>
    f.rpc.fireAndForget()

    verify(f.mockRpc, never()).fireAndForget()
  }

  it should "not call any remote method when only getters are called" in fixture { f =>
    f.rpc.inner

    verifyZeroInteractions(f.mockRpc, f.mockInnerRpc)
  }

  it should "call proper method of inner rpc" in fixture { f =>
    when(f.mockRpc.inner).thenReturn(f.mockInnerRpc)
    val mockMethodCall = registerForUnitMethod(f.mockInnerRpc.innerFire())

    f.rpc.inner.innerFire()

    whenReady(mockMethodCall) { _ =>
      verify(f.mockInnerRpc).innerFire()
    }
  }

  it should "successfully call future method" in fixture { f =>
    when(f.mockRpc.echoAsString(5)).thenReturn(Future.successful("5"))
    val result = f.rpc.echoAsString(5)

    result.futureValue shouldBe "5"
  }

  it should "wrap remote exception with RemoteCallException" in fixture { f =>
    when(f.mockRpc.echoAsString(anyInt())).thenReturn(Future.failed(new NullPointerException))

    whenFailed(f.rpc.echoAsString(5)) { thrown =>
      thrown shouldBe a[RemoteCallException]
      thrown.getMessage should include("NullPointerException")
    }
  }

  it should "return RemoteTimeoutException when no connection to the remote RPC" in noConnectionFixture { f =>
    whenFailed(f.rpc.echoAsString(4), timeout(Span(5, Seconds))) { thrown =>
      thrown shouldBe a[RemoteTimeoutExceptionType]
    }
  }

  it should "successfully call observable method" in fixture { f =>
    when(f.mockRpc.stream).thenReturn(Observable.from(1, 2, 3, 4, 5))

    val result = f.rpc.stream.asFutureSeq

    whenReady(result) { value =>
      value should contain inOrderOnly(1, 2, 3, 4, 5)
    }
  }

  it should "wrap remote exception with RemoteCallException in observable" in fixture { f =>
    when(f.mockRpc.stream).thenReturn(Observable.error(new IllegalStateException))

    whenFailed(f.rpc.stream.asExistingFuture) { thrown =>
      thrown shouldBe a[RemoteCallException]
      thrown.getMessage should include("IllegalStateException")
    }

  }

  it should "return RemoteTimeoutException when no connection to the remote RPC when observable method called" in noConnectionFixture { f =>
    f.rpc.stream.error.asExistingFuture.futureValue(timeout(Span(2, Seconds))) shouldBe a[RemoteTimeoutExceptionType]
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    system.terminate()
  }

}

object AkkaRPCFrameworkTest {
  type RemoteTimeoutExceptionType = RemoteTimeoutException.type

  private def whenFailed[T, U](future: Future[T])(fun: Exception => U): U = {
    whenFailedGeneric(future, w => w.await())(fun)
  }

  private def whenFailed[T, U](future: Future[T], timeout: PatienceConfiguration.Timeout)(fun: Exception => U): U = {
    whenFailedGeneric(future, w => w.await(timeout))(fun)
  }

  private[this] def whenFailedGeneric[T, U](future: Future[T], awaitCode: Waiter => Any)(fun: Exception => U): U = {
    val w = new Waiter
    future onComplete {
      case Failure(e) => w(throw e); w.dismiss()
      case Success(_) => w.dismiss()
    }
    fun(Matchers.intercept[Exception] {
      awaitCode(w)
    })
  }


  /**
    * Registers for execution of mock unit method. Returned future will be completed if only passed mock method will be called.
    *
    * Example:
    * {{{
    *   val mockMethodCall = registerForUnitMethod(f.mockRpc.fireAndForget())
    *
    *   f.rpcFireAndForget()
    *
    *   whenReady(mockMethodCall) { res =>
    *     verify(f.mockRpc).fireAndForget()   //call to verify that method was called exactly once
    *   }
    * }}}
    *
    * This is ugly hack, but necessary for being notified if remote method has been called.
    * First idea was to write test code like:
    * {{{
    *   f.rpc.fireAndForget()
    *   verify(f.mockRpc).fireAndForget
    * }}}
    *
    * Unfortunately it results in non-deterministic behaviour, as messages between actors are sent in another thread,
    * and sometimes it takes longer to deliver Akka messages than to reach to verify method
    *
    * @param mockCode method called on mock which returns Unit
    * @return Future which will be completed with Unit if only passed method were called
    */
  private def registerForUnitMethod(mockCode: => Unit): Future[Unit] = {
    val calledMethodPromise = Promise[Unit]
    when(mockCode).thenAnswer(new Answer[Unit] {
      override def answer(invocation: InvocationOnMock): Unit = calledMethodPromise.trySuccess(Unit)
    })
    calledMethodPromise.future
  }

  private implicit class ObservableOps[T](private val observable: Observable[T]) extends AnyVal {
    def asFutureSeq: Future[Seq[T]] = {
      observable.foldLeft(List.empty[T]) {
        case (list, elem) => elem :: list
      }.map(_.reverse).asFuture.map(_.get)
    }

    def asExistingFuture: Future[T] = {
      observable.asFuture.map(_.get)
    }

  }

}