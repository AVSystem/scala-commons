# Redis client

The module `commons-redis` contains from-the-scratch implementation of Scala driver for Redis. It's most important goals
and characteristics are:
* non-blocking network communication (based on Akka IO)
* asynchronous API
* support for Redis Cluster
* type safety
* functional(ish) design
* flexibility and genericity of API
* good performance

Features: 
* full Redis 3.2 API coverage (except for blocking commands, pub/sub API and some unsafe or debugging 
  commands like `MONITOR`)
* three client implementations: for single connection (`RedisConnectionClient`), connection pool to a single node 
  (`RedisNodeClient`) and for Redis Cluster (`RedisClusterClient`), each client supporting only appropriate API subset
  (e.g. you can't execute unkeyed commands using Redis Cluster client)
* support for pipelining, with functional design (composable `RedisBatch` objects)
* dedicated support for atomic transactions (every `RedisBatch` may be easily wrapped into a `MULTI`/`EXEC` block)
* dedicated support for transactions with optimistic locking (multiple `RedisBatch` objects may be composed into 
  multi-stage `RedisOp` objects which may contain `WATCH`/`UNWATCH` commands and `MULTI`/`EXEC` blocks)
* various API flavors (raw API that creates `RedisBatch` objects, asynchronous API returning `Future`s and blocking API)
* strong conceptual separation between "Redis API" and "Redis client", e.g. multiple API flavors may reuse the same client
* customizable key, hash key and value types for every API flavor with automatic serialization powered by `GenCodec`
* genericity - ability to easily create your own API flavors

Missing features:
* Support for regular master-slave replication and Redis Sentinel (in favor of Redis Cluster)
* Publish/Subscribe
* Blocking commands (`BLPOP`, `BRPOP`, `BRPOPLPUSH`)
