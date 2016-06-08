package com.avsystem.commons

/**
  * Author: ghik
  * Created: 06/06/16.
  */
package object redis {
  type Scoped[S] = {
    type Command[+A] = RedisCommand[A, S]
    type Batch[+A] = RedisBatch[A, S]
    type Op[+A] = RedisOp[A, S]
  }

  type ConnectionBatch[+A] = RedisBatch[A, Scope.Connection]
  type NodeBatch[+A] = RedisBatch[A, Scope.Node]
  type ClusterBatch[+A] = RedisBatch[A, Scope.Cluster]

  type ConnectionCommand[+A] = RedisCommand[A, Scope.Connection]
  type NodeCommand[+A] = RedisCommand[A, Scope.Node]
  type ClusterCommand[+A] = RedisCommand[A, Scope.Cluster]

  type ConnectionOp[+A] = RedisOp[A, Scope.Connection]
  type NodeOp[+A] = RedisOp[A, Scope.Node]
  type ClusterOp[+A] = RedisOp[A, Scope.Cluster]
}
