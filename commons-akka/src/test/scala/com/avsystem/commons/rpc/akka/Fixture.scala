package com.avsystem.commons
package rpc.akka

/**
  * @author Wojciech Milewski
  */
final case class Fixture(rpc: TestRPC, mockRpc: TestRPC, mockInnerRpc: InnerRPC)