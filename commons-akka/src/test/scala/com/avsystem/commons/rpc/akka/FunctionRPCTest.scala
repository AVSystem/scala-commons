package com.avsystem.commons
package rpc.akka

import akka.pattern.AskTimeoutException
import org.mockito.ArgumentMatchers._
import org.mockito.Mockito._
import org.scalatest.time.Span

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

/**
  * @author Wojciech Milewski
  */
trait FunctionRPCTest {this: RPCFrameworkTest =>

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
    val span = Span.convertDurationToSpan(callTimeout.plus(1.second))
    whenFailed(f.rpc.echoAsString(4), timeout(span)) { thrown =>
      thrown shouldBe an[AskTimeoutException]
    }
  }

  it should "call several methods" in fixture { f =>
    when(f.mockRpc.echoAsString(anyInt())).thenReturn(Future.successful("1"))

    f.rpc.echoAsString(1).futureValue shouldBe "1"
    f.rpc.echoAsString(1).futureValue shouldBe "1"
    f.rpc.echoAsString(1).futureValue shouldBe "1"
  }
}
