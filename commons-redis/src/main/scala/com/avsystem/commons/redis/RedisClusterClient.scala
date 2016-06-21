package com.avsystem.commons
package redis

import java.io.Closeable

import akka.actor.ActorSystem

/**
  * Author: ghik
  * Created: 13/06/16.
  */
final class RedisClusterClient(seedNodes: List[NodeAddress] = List(NodeAddress.Default), poolSize: Int = 1)
  (implicit system: ActorSystem) extends Closeable {

  def close() = ()
}
