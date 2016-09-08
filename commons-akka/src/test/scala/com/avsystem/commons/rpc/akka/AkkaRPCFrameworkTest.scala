package com.avsystem.commons
package rpc.akka

import java.util.concurrent.atomic.AtomicLong

import akka.actor.{ActorPath, ActorSystem, Inbox, Terminated}
import org.mockito.Mockito.when
import org.scalatest.{BeforeAndAfterAll, FlatSpec}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/**
  * @author Wojciech Milewski
  */
abstract class AkkaRPCFrameworkTest(
  serverSystem: ActorSystem,
  clientSystem: ActorSystem,
  serverSystemPath: Option[String] = None)
  extends FlatSpec with RPCFrameworkTest with ProcedureRPCTest with FunctionRPCTest with GetterRPCTest with ObservableRPCTest with BeforeAndAfterAll {

  /**
    * Servers as identifier supplier for each test case to allow tests parallelization.
    */
  private val idCounter = new AtomicLong()

  override def fixture(testCode: Fixture => Any): Unit = {
    val id: Long = idCounter.getAndIncrement()
    val serverActorName = s"rpcServerActor$id"

    val testRpcMock = mock[TestRPC]
    val innerRpcMock = mock[InnerRPC]
    val serverActor = {
      implicit val system = serverSystem
      AkkaRPCFramework.serverActor[TestRPC](testRpcMock, AkkaRPCServerConfig(actorName = serverActorName))
    }
    val rpc = {
      implicit val system = clientSystem
      AkkaRPCFramework.client[TestRPC](AkkaRPCClientConfig(serverPath = serverSystemPath.fold(serverActor.path)(
        serverPath => ActorPath.fromString(s"$serverPath/user/$serverActorName")))
      )
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
      serverPath = serverSystemPath.fold(ActorPath.fromString("akka://user/thisactorshouldnotexists"))(serverPath => ActorPath.fromString(s"$serverPath/user/thisactorshouldnotExist)"))))
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