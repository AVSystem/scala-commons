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

AkkaRPCFramework uses `com.avsystem.commons.serialization.GenCodec` as a serialization method from your entities to
implemented raw value. In order to make a class allowed in RPC method, you need to provide an implicit `GenCodec` instance.

Akka default configuration uses Java Serialization to serialize messages send via remoting.
Implementation of the framework provides custom serializer of internal messages (faster than default one),
but unfortunately you need to enable it manually.

Full setup of the server with faster serializer configured:
```hocon
akka {
  actor {
    provider = "akka.remote.RemoteActorRefProvider"
    serializers {
      rpcInternal = com.avsystem.commons.rpc.akka.serialization.RemoteMessageSerializer
    }
    serialization-bindings {
        "com.avsystem.commons.rpc.akka.RemoteMessage" = rpcInternal
    }
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
The same serializer must be configured on the client side.
You can learn more about Akka serialization in [Akka documentation](http://doc.akka.io/docs/akka/current/scala/serialization.html#Configuration)