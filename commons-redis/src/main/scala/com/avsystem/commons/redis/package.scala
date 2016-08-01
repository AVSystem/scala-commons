package com.avsystem.commons

/**
  * Author: ghik
  * Created: 06/06/16.
  */
package object redis {
  type ConnectionBatch[+A] = RedisBatch[A, Scope.Connection]
  type OperationBatch[+A] = RedisBatch[A, Scope.Operation]
  type NodeBatch[+A] = RedisBatch[A, Scope.Node]
  type ClusterBatch[+A] = RedisBatch[A, Scope.Cluster]

  type ConnectionCommand[+A] = RedisCommand[A, Scope.Connection]
  type OperationCommand[+A] = RedisCommand[A, Scope.Operation]
  type NodeCommand[+A] = RedisCommand[A, Scope.Node]
  type ClusterCommand[+A] = RedisCommand[A, Scope.Cluster]
}
