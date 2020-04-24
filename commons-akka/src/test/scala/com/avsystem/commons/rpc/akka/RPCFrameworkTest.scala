package com.avsystem.commons
package rpc.akka

import org.mockito.Mockito._
import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.Answer
import org.scalatest.concurrent.Waiters.Waiter
import org.scalatest.concurrent.{PatienceConfiguration, ScalaFutures}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.Span

import scala.concurrent.duration._

/**
  * @author Wojciech Milewski
  */
trait RPCFrameworkTest extends AnyFlatSpecLike with Matchers with ScalaFutures {

  import RPCFrameworkTest._

  val callTimeout: FiniteDuration = 200.millis

  override implicit val patienceConfig: PatienceConfig =
    PatienceConfig(timeout = Span.convertDurationToSpan(10.seconds))

  /**
    * Run tests with connection between client and server.
    *
    * Should do necessary cleanup after each test.
    */
  def fixture(testCode: Fixture => Any): Unit

  /**
    * Runs tests without any connection between client and server.
    *
    * Should do necessary cleanup after each test.
    *
    */
  def noConnectionFixture(testCode: Fixture => Any): Unit

  protected final def whenFailed[T, U](future: Future[T])(fun: Exception => U)(implicit ec: ExecutionContext): U = {
    whenFailedGeneric(future, w => w.await())(fun)
  }

  protected final def whenFailed[T, U](future: Future[T], timeout: PatienceConfiguration.Timeout)(fun: Exception => U)(implicit ec: ExecutionContext): U = {
    whenFailedGeneric(future, w => w.await(timeout))(fun)
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
  protected final def registerForUnitMethod(mockCode: => Unit): Future[Unit] = registerForCallWithReturnValue[Unit](mockCode, Unit)

  protected final def registerForCallWithReturnValue[T](mockCode: => T, returnValue: => T): Future[Unit] = {
    val calledMethodPromise = Promise[Unit]
    when(mockCode).thenAnswer(new Answer[T] {
      override def answer(invocation: InvocationOnMock): T = {
        calledMethodPromise.trySuccess(Unit)
        returnValue
      }
    })
    calledMethodPromise.future
  }


}

object RPCFrameworkTest {

  private def whenFailedGeneric[T, U](future: Future[T], awaitCode: Waiter => Any)(fun: Exception => U)(implicit ec: ExecutionContext): U = {
    val w = new Waiter
    future onComplete {
      case Failure(e) => w(throw e); w.dismiss()
      case Success(_) => w.dismiss()
    }
    fun(Matchers.intercept[Exception] {
      awaitCode(w)
    })
  }
}
