package com.avsystem.commons
package rpc.akka

import org.mockito.Mockito._

/**
  * @author Wojciech Milewski
  */
trait GetterRPCTest {this: RPCFrameworkTest =>
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
}
