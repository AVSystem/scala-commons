package com.avsystem.commons
package rpc.akka

import akka.actor.{ActorPath, ActorSystem, Inbox, Terminated}
import org.scalatest.{BeforeAndAfterAll, FlatSpec}
import org.mockito.Mockito.{when, reset}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

/**
  * @author Wojciech Milewski
  */
abstract class AkkaRPCFrameworkTest(serverSystem: ActorSystem, clientSystem: ActorSystem, existingPath: Option[ActorPath] = None, nonExistingPath: Option[ActorPath] = None)
  extends FlatSpec with RPCFrameworkTest with ProcedureRPCTest with FunctionRPCTest with GetterRPCTest with ObservableRPCTest with BeforeAndAfterAll {

  override def fixture(testCode: Fixture => Any): Unit = {
    val testRpcMock = mock[TestRPC]
    val innerRpcMock = mock[InnerRPC]
    val serverActor = {
      implicit val system = serverSystem
      AkkaRPCFramework.serverActor[TestRPC](testRpcMock)
    }
    val rpc = {
      implicit val system = clientSystem
      AkkaRPCFramework.client[TestRPC](AkkaRPCClientConfig(serverPath = existingPath.getOrElse(serverActor.path)))
    }

    try {
      testCode(Fixture(rpc = rpc, mockRpc = testRpcMock, mockInnerRpc = innerRpcMock))
    } finally {
      val inbox = Inbox.create(serverSystem)
      inbox.watch(serverActor)
      serverSystem.stop(serverActor)
      inbox.receive(2.seconds) match {
        case Terminated(_) =>
      }
    }
  }

  override def noConnectionFixture(testCode: Fixture => Any): Unit = {
    implicit val system = clientSystem
    val mockRpc = mock[TestRPC]
    val mockInnerRpc = mock[InnerRPC]
    val rpc = AkkaRPCFramework.client[TestRPC](AkkaRPCClientConfig(
      functionCallTimeout = callTimeout,
      observableMessageTimeout = callTimeout,
      serverPath = nonExistingPath.getOrElse(ActorPath.fromString("akka://user/thisactorshouldnotexists"))))
    testCode(Fixture(rpc = rpc, mockRpc = mockRpc, mockInnerRpc = mockInnerRpc))
  }


  override protected def beforeAll(): Unit = {
    super.beforeAll()
    /*
     * Kind of warmup of Akka systems.
     * First RPC request can be very slow due to connection establishment, done especially by Akka Remote.
     */
    fixture { f =>
      when(f.mockRpc.echoAsString(0)).thenReturn(Future.successful(""))
      Await.ready(f.rpc.echoAsString(0), 1.second)
    }
  }
  override protected def afterAll(): Unit = {
    super.afterAll()
    serverSystem.terminate()
    clientSystem.terminate()
    Await.ready(serverSystem.whenTerminated, 30.seconds)
    Await.ready(clientSystem.whenTerminated, 30.seconds)
  }

}