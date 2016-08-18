package com.avsystem.commons
package rpc.akka

import java.util.concurrent.atomic.AtomicInteger

import akka.actor.{ActorPath, ActorSystem}
import com.avsystem.commons.rpc.RPC
import com.typesafe.config.{Config, ConfigFactory}
import monifu.reactive.Observable
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, Scope, Setup, State, TearDown, Warmup}
import org.openjdk.jmh.infra.Blackhole

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

/**
  * @author Wojciech Milewski
  */
@Warmup(iterations = 7)
@Measurement(iterations = 20)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Thread)
class AkkaRPCFrameworkBenchmark {

  import AkkaRPCFrameworkBenchmark._

  @Benchmark
  def remoteCall(serverState: ServerState, client: ClientState): Unit = {
    Await.result(client.rpc.echoAsString(5), 1.minute)
  }


}

object AkkaRPCFrameworkBenchmark {
  val cpuTokens = 4096

  private val serverPort = 2552
  private val clientPortCounter = new AtomicInteger(serverPort + 1)

  @State(Scope.Benchmark)
  class ServerState {
    private implicit var serverSystem: ActorSystem = _

    private val rpc: TestRPC = new TestRPC {
      override def echoAsString(int: Int): Future[String] = Future {
        Blackhole.consumeCPU(cpuTokens)
        int.toString
      }
      override def fireAndForget(): Unit = {
        Blackhole.consumeCPU(cpuTokens)
      }
      override def inner: InnerRPC = new InnerRPC {
        override def innerFire(): Unit = {
          Blackhole.consumeCPU(cpuTokens)
        }
      }
      override def stream: Observable[Int] = {
        Blackhole.consumeCPU(cpuTokens)
        Observable.from(1, 2, 3, 4, 5)
      }
    }

    @Setup
    def setup(): Unit = {
      serverSystem = ActorSystem("Server", config(serverPort))

      AkkaRPCFramework.serverActor[TestRPC](rpc)
    }

    @TearDown
    def teardown(): Unit = {
      Await.ready(serverSystem.terminate(), 30.seconds)
    }
  }

  @State(Scope.Thread)
  class ClientState {
    private implicit var clientSystem: ActorSystem = _

    var rpc: TestRPC = _

    @Setup
    def setup(): Unit = {
      clientSystem = ActorSystem("Client", config(clientPortCounter.getAndIncrement()))

      rpc = AkkaRPCFramework.client[TestRPC](AkkaRPCClientConfig(ActorPath.fromString("akka.tcp://Server@127.0.0.1:2552/user/rpcServerActor")))
    }

    @TearDown
    def teardown(): Unit = {
      Await.ready(clientSystem.terminate(), 30.seconds)
    }
  }

  def config(port: Int): Config = {
    ConfigFactory.parseString(
      s"""
         |akka {
         |  actor {
         |    provider = "akka.remote.RemoteActorRefProvider"
         |    serializers {
         |      remoteMessage = com.avsystem.commons.rpc.akka.serialization.RemoteMessageSerializer
         |    }
         |    serialization-bindings {
         |      "com.avsystem.commons.rpc.akka.RemoteMessage" = remoteMessage
         |    }
         |  }
         |  remote {
         |    enabled-transports = ["akka.remote.netty.tcp"]
         |    netty.tcp {
         |      hostname = "127.0.0.1"
         |      port = $port
         |    }
         |  }
         |}
       """.stripMargin)
  }

}

@RPC
trait TestRPC {
  def fireAndForget(): Unit
  def echoAsString(int: Int): Future[String]
  def stream: Observable[Int]
  def inner: InnerRPC
}

@RPC
trait InnerRPC {
  def innerFire(): Unit
}