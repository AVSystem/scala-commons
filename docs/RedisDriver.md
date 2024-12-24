# Redis driver

**⚠️ WARNING: This module is deprecated and scheduled for removal in a future release. Please migrate to an alternative
solution.**

`commons-redis` - Scala driver for Redis

```scala
libraryDependencies += "com.avsystem.commons" %% "commons-redis" % avsCommonsVersion
```

**[API reference](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/redis/index.html)**

**[Quickstart example](https://github.com/AVSystem/scala-commons/blob/master/commons-redis/src/test/scala/com/avsystem/commons/redis/examples/NodeClientExample.scala)**

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [Redis driver](#redis-driver)
  - [Overview](#overview)
    - [Quickstart example](#quickstart-example)
    - [APIs and clients](#apis-and-clients)
    - [Client types](#client-types)
    - [API variants](#api-variants)
  - [Examples](#examples)
  - [Benchmarks](#benchmarks)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Overview

The module `commons-redis` contains from-the-scratch implementation of Scala driver for Redis. Its most important goals
and characteristics are:
* non-blocking network communication (based on Pekko IO)
* asynchronous API
* support for Redis Cluster
* type safety
* functional(ish) design
* flexibility and genericity of API
* good performance

Features: 
* Redis 5 API support, excluding pub/sub, `MEMORY` commands and some unsafe/debugging commands like `MONITOR`.
* four client implementations: for single connection ([`RedisConnectionClient`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/redis/RedisConnectionClient.html)), 
  connection pool to a single node ([`RedisNodeClient`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/redis/RedisNodeClient.html)),
  for master-slave installations with Redis Sentinel ([`RedisMasterSlaveClient`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/redis/RedisMasterSlaveClient.html))
  and for Redis Cluster ([`RedisClusterClient`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/redis/RedisClusterClient.html)), 
  each client supporting only appropriate API subset.
* TLS support (requires Redis 6 with TLS support enabled)
* support for batching (pipelining), with functional design (composable [`RedisBatch`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/redis/RedisBatch.html) objects)
  and automatic distribution of batch contents over multiple Redis Cluster masters
* dedicated support for atomic transactions (every [`RedisBatch`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/redis/RedisBatch.html) 
  may be easily wrapped into a `MULTI`/`EXEC` block)
* dedicated support for transactions with optimistic locking (multiple [`RedisBatch`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/redis/RedisBatch.html) 
  objects may be composed into multi-stage [`RedisOp`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/redis/RedisOp.html) 
  objects which may contain `WATCH`/`UNWATCH` commands and `MULTI`/`EXEC` blocks)
* various API flavors (raw API that creates [`RedisBatch`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/redis/RedisBatch.html) 
  objects, asynchronous API returning `Future`s and blocking API)
* strong conceptual separation between "Redis API" and "Redis client", e.g. multiple API flavors may reuse the same client
* customizable key, hash key and value types for every API flavor with automatic serialization powered by 
  [`GenCodec`](GenCodec.md)
* genericity - ability to easily create your own API flavors

Missing features:
* Publish/Subscribe
* Full Redis 6 support (RESP3 protocol, new commands)

### Quickstart example

```scala
import org.apache.pekko.actor.ActorSystem
import com.avsystem.commons.redis._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util._

implicit val actorSystem = ActorSystem()
val client = new RedisNodeClient
val api = RedisApi.Node.Async.StringTyped(client)

api.get("key").onComplete {
  case Success(Opt(value)) => println(s"Got value $value")
  case Success(Opt.Empty) => println(s"Got no value")
  case Failure(t) => t.printStackTrace()
}
```

More examples can be found in [test sources](https://github.com/AVSystem/scala-commons/tree/master/commons-redis/src/test/scala/com/avsystem/commons/redis/examples).

### APIs and clients

AVSystem Redis driver makes a clear distinction between a _client_ and an _API_. In order to talk to Redis, you'll
need both of these.

A _client_ is an object whose responsibility is to maintain connection(s) to Redis server(s) and communicate with it.
However, clients don't directly expose Redis commands as plain Scala API. This is because that API can come in many
flavors and therefore, it's implemented by separate object. 

Clients and APIs are very loosely coupled. Thanks to that, you can use multiple API flavors with the same client and
reuse allocated network resources.

### Client types

AVSystem Redis driver comes with three client types.
* [`RedisConnectionClient`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/redis/RedisConnectionClient.html) 
  uses a single, non-reconnectable connection to a Redis instance. This type of client can execute all
  commands available in the driver. This includes commands which change state of the connection, e.g. `CLIENT SETNAME`.
  Because such commands can be executed, this type of client will _not_ try to reconnect when the connection is lost
  (and its state along with it). [`RedisConnectionClient`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/redis/RedisConnectionClient.html) 
  instances must be manually recreated after connection
  failures. If you need a single-connection client which automatically reconnects, you might use [`RedisConnectionClient`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/redis/RedisNodeClient.html) configured
  with connection pool size equal to 1, but you won't be able to invoke connection-state-changing commands on it
  (except for initialization, e.g. `AUTH`)
* [`RedisNodeClient`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/redis/RedisNodeClient.html) uses 
  a fixed-size round-robin connection pool to a single Redis instance. It can execute almost all 
  commands available in the driver except for the ones which change connection state. When connections are lost, they are
  automatically reconnected, using an exponential backoff procedure (each consecutive reconnection attempt is appropriately
  delayed to avoid too many reconnection attempts when node is down).
* [`RedisMasterSlaveClient`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/redis/RedisMasterSlaveClient.html)
  connects to a Redis Master/Slave installation with Redis Sentinels. It maintains connections to all known Sentinels
  and uses them to obtain and monitor the address of the current master node. Internally, a [`RedisNodeClient`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/redis/RedisNodeClient.html)
  is allocated for the master node. Master/Slave client can execute the same set of commands as `RedisNodeClient`.
* [`RedisClusterClient`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/redis/RedisClusterClient.html) 
  connects to a Redis Cluster deployment. It uses dynamically allocated [`RedisNodeClient`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/redis/RedisNodeClient.html) instances
  for every cluster master known at given moment. Cluster state is monitored with separate monitoring connections.
  Cluster client can only execute commands which contain keys. It automatically dispatches every command to appropriate
  master. It is also possible to gain direct access to individual master node clients.
  
### API variants

API objects provide you with plain Scala API where every method roughly corresponds to a Redis native command, e.g.
there's `get` method for Redis `GET` command. However, the exact signature of these methods depends on the API variant
you are using. They can differ in following ways:

* the subset of commands supported

  Some API objects are directly associated with client instances. Since different clients support different subsets
  of commands (e.g. [`RedisClusterClient`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/redis/RedisClusterClient.html) 
  can only execute keyed commands), this limitation is also reflected in methods
  available in API objects which use these clients.
* method result types

  An API object might by asynchronous and return `Future`s but it may also be synchronous and return result of every
  method directly (without wrapping into a `Future`). There are also API variants which return command results as 
  "unexecuted" [`RedisBatch`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/redis/RedisBatch.html) 
  objects that need to be manually passed to the client for execution.
* representation of keys, hash fields, values and records

  Redis internally stores keys, has fields and data as arbitrary byte sequences, but on Scala level we don't usually want
  to deal with raw binary data. Therefore, the driver allows you to use any types as long as you specify how they are 
  serialized to binary form. Every API object is bound to particular key type, hash key type and value type.
  
As you can see, even though every API variant provides a `get` method, its exact signature is not determined until
result type, key type and value type are chosen.

The "generic" signature of `get` looks like this:

```scala
def get(key: Key): Result[Opt[Value]]
```

But for concrete API variant, e.g. `RedisApi.Keyed.Async.StringTyped` (asynchronous keyed commands with keys and values 
represented as `String`) it looks like this:

```scala
def get(key: String): Future[Opt[String]]
```

Predefined API variants are defined in [`RedisApi`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/redis/RedisApi$.html)
object. Consult its documentation for more details.

## Examples

Examples can be found in [test sources](https://github.com/AVSystem/scala-commons/tree/master/commons-redis/src/test/scala/com/avsystem/commons/redis/examples).

* quickstart, simple node client usage - [NodeClientExample](https://github.com/AVSystem/scala-commons/blob/master/commons-redis/src/test/scala/com/avsystem/commons/redis/examples/NodeClientExample.scala)
* API customization, serialization - [ApiCustomizationExample](https://github.com/AVSystem/scala-commons/blob/master/commons-redis/src/test/scala/com/avsystem/commons/redis/examples/ApiCustomizationExample.scala)
* connection client usage - [ConnectionClientExample](https://github.com/AVSystem/scala-commons/blob/master/commons-redis/src/test/scala/com/avsystem/commons/redis/examples/ConnectionClientExample.scala)
* master/slave client usage - [MasterSlaveClientExample](https://github.com/AVSystem/scala-commons/blob/master/commons-redis/src/test/scala/com/avsystem/commons/redis/examples/MasterSlaveClientExample.scala)
* cluster client usage - [ClusterClientExample](https://github.com/AVSystem/scala-commons/blob/master/commons-redis/src/test/scala/com/avsystem/commons/redis/examples/ClusterClientExample.scala)
* authentication, database selection - [ConnectionSetupExample](https://github.com/AVSystem/scala-commons/blob/master/commons-redis/src/test/scala/com/avsystem/commons/redis/examples/ConnectionSetupExample.scala)
* pipelining - [PipeliningExample](https://github.com/AVSystem/scala-commons/blob/master/commons-redis/src/test/scala/com/avsystem/commons/redis/examples/PipeliningExample.scala)
* simple atomic transactions (without `WATCH`) - [MultiExecExample](https://github.com/AVSystem/scala-commons/blob/master/commons-redis/src/test/scala/com/avsystem/commons/redis/examples/MultiExecExample.scala)
* transactions with `WATCH` and optimistic locking - [TransactionExample](https://github.com/AVSystem/scala-commons/blob/master/commons-redis/src/test/scala/com/avsystem/commons/redis/examples/TransactionExample.scala)
* LUA scripting - [ScriptingExample](https://github.com/AVSystem/scala-commons/blob/master/commons-redis/src/test/scala/com/avsystem/commons/redis/examples/ScriptingExample.scala)

## Benchmarks

Some simple JMH benchmarks have been implemented in 
[`RedisClientBenchmark`](https://github.com/AVSystem/scala-commons/blob/master/commons-benchmark/src/main/scala/com/avsystem/commons/redis/RedisClientBenchmark.scala)

Here's an example result:

```
[info] Benchmark                                                     Mode  Cnt        Score       Error  Units
[info] RedisClientBenchmark.clusterClientBatchBenchmark             thrpt   40   972566.875 ± 23690.312  ops/s
[info] RedisClientBenchmark.clusterClientCommandBenchmark           thrpt   40   321375.083 ± 50150.155  ops/s
[info] RedisClientBenchmark.clusterClientDistributedBatchBenchmark  thrpt   40   762133.038 ± 18547.533  ops/s
[info] RedisClientBenchmark.clusterClientOperationBenchmark         thrpt   40    25720.818 ±   949.894  ops/s
[info] RedisClientBenchmark.clusterClientTransactionBenchmark       thrpt   40   993998.179 ± 30108.538  ops/s
[info] RedisClientBenchmark.connectionClientBatchBenchmark          thrpt   40  1079675.683 ± 14930.129  ops/s
[info] RedisClientBenchmark.connectionClientCommandBenchmark        thrpt   40   714678.686 ± 42471.005  ops/s
[info] RedisClientBenchmark.connectionClientOperationBenchmark      thrpt   40    13692.019 ±   563.235  ops/s
[info] RedisClientBenchmark.connectionClientTransactionBenchmark    thrpt   40   805744.265 ± 26065.241  ops/s
[info] RedisClientBenchmark.nodeClientBatchBenchmark                thrpt   40   935836.711 ± 19358.894  ops/s
[info] RedisClientBenchmark.nodeClientCommandBenchmark              thrpt   40   684494.740 ± 44997.728  ops/s
[info] RedisClientBenchmark.nodeClientOperationBenchmark            thrpt   40    24945.317 ±   920.142  ops/s
[info] RedisClientBenchmark.nodeClientTransactionBenchmark          thrpt   40   735208.728 ± 23660.125  ops/s
```

Hardware & environment:
* Intel(R) Core(TM) i7-3770 CPU @ 3.40GHz
* Ubuntu 14.04, Linux 4.4.0 x86_64
* Oracle Java 1.8.0_111-b14, Scala 2.11.8
* Redis 3.2 running on the same machine
* For Redis Cluster benchmarks - 3 masters running on the same machine
