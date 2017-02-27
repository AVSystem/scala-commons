package com.avsystem.commons
package rpc.akka

import org.mockito.Mockito._

/**
  * @author Wojciech Milewski
  */
trait ProcedureRPCTest {this: RPCFrameworkTest =>

  it should "successfully call unit method" in fixture { f =>
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

}
