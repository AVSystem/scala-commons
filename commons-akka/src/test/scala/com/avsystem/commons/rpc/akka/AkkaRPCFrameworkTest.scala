package com.avsystem.commons
package rpc.akka

import akka.actor.{ActorPath, ActorSystem, Inbox, Terminated}
import akka.stream.ActorMaterializer
import org.scalatest.FlatSpec

import scala.concurrent.Await
import scala.concurrent.duration._

/**
  * @author Wojciech Milewski
  */
abstract class AkkaRPCFrameworkTest(serverSystem: ActorSystem, clientSystem: ActorSystem, existingPath: Option[ActorPath] = None, nonExistingPath: Option[ActorPath] = None)
  extends FlatSpec with RPCFrameworkTest with ProcedureRPCTest with FunctionRPCTest with GetterRPCTest with ObservableRPCTest {

  override def fixture(testCode: Fixture => Any): Unit = {
    val testRpcMock = mock[TestRPC]
    val innerRpcMock = mock[InnerRPC]
    val serverActor = {
      implicit val system = serverSystem
      AkkaRPCFramework.serverActor[TestRPC](testRpcMock)
    }
    val rpc = {
      implicit val system = clientSystem
      implicit val materializer = ActorMaterializer()
      AkkaRPCFramework.client[TestRPC](AkkaRPCClientConfig(serverPath = existingPath.getOrElse(serverActor.path)))
    }
    try {
      testCode(Fixture(rpc = rpc, mockRpc = testRpcMock, mockInnerRpc = innerRpcMock))
    } finally {
      //todo make it sync, because it sometimes fails
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
    implicit val materializer = ActorMaterializer()
    val mockRpc = mock[TestRPC]
    val mockInnerRpc = mock[InnerRPC]
    val rpc = AkkaRPCFramework.client[TestRPC](AkkaRPCClientConfig(
      functionCallTimeout = callTimeout,
      observableMessageTimeout = callTimeout,
      serverPath = nonExistingPath.getOrElse(ActorPath.fromString("akka://user/thisactorshouldnotexists"))))
    testCode(Fixture(rpc = rpc, mockRpc = mockRpc, mockInnerRpc = mockInnerRpc))
  }


  override protected def afterAll(): Unit = {
    super.afterAll()
    serverSystem.terminate()
    clientSystem.terminate()
    Await.ready(serverSystem.whenTerminated, 30.seconds)
    Await.ready(clientSystem.whenTerminated, 30.seconds)
  }

}