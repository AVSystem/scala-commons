# Akka based typesafe RPC framework

`commons-akka` module contains implementation of RPC framework using Akka Remoting. It supports every basic method type (procedure, function and getter), but it also allows you to define methods returning `Observable` from Monifu library.

Note: For examples purpose, let's assume that we have defined rpc:
```scala
@RPC trait UserService {
  def fireAndForget: Unit
  def callMeMaybe(phoneNumber: String): Future[Boolean]
  def producer: ProducerService
}

@RPC trait ProducerService {
  def streamData: Observable[Int]
}

class UserServiceImpl extends UserService {
  //implementation...
}
```

## Setup

### Server-side setup

In order to run server with default configuration, you need to call:
```scala
implicit val actorSystem: ActorSystem = ActorSystem("ServerSystem")

val rpcImplementation: UserService = new UserServiceImpl()

AkkaRPCFramework.serverActor[UserService](rpcImplementation)
```

Implicit actor system should be configured properly to work as remote system. How to properly configure it, see in
[Akka documentation](http://doc.akka.io/docs/akka/current/scala/remoting.html#Preparing_your_ActorSystem_for_Remoting).

Example configuration of Akka remote system:
```hocon
akka {
  actor {
    provider = "akka.remote.RemoteActorRefProvider"
  }
  remote {
    enabled-transports = ["akka.remote.netty.tcp"]
    netty.tcp {
      hostname = "127.0.0.1"
      port = 2552
    }
  }
}
```

Running server with such configuration will spawn an actor which path is:
`akka.tcp//ServerSystem@127.0.0.1:2552/user/rpcServerActor`

Name of spawned actor can be customized during initialization of framework, by passing changed instance of `AkkaRPCServerConfig` to the `AkkaRPCFramework.serverActor` method.

### Client-side setup
In order to run client with default configuration, you need to call:
```scala
implicit val actorSystem: ActorSystem = ActorSystem("ClientSystem")

val rpc: TestRPC = AkkaRPCFramework.client[TestRPC](AkkaRPCClientConfig(serverPath = "akka.tcp//ServerSystem@127.0.0.1:2552/user/rpcServerActor"))
```

Similarly to server, implicit actor system should be configured properly to work as remote system.

As a `serverPath` you need to pass proper remote actor path, which is listening on the server.

## Serialization

