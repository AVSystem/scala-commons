package com.avsystem.commons
package redis.examples

import org.apache.pekko.actor.ActorSystem
import com.avsystem.commons.redis._

// Global execution context is used for the sake of simplicity of this example,
// think well if this is what you actually want.
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Basic example showing how to execute simple command on [[RedisClusterClient]].
  */
object ClusterClientExample extends App {
  implicit val actorSystem: ActorSystem = ActorSystem()

  // The cluster client asks seed nodes about cluster state (by default local Redis instance is the only seed node)
  // and then uses separate RedisNodeClients to connect individually to every master mentioned in cluster state
  // that holds some data.
  val client = new RedisClusterClient
  // We can only execute keyed commands on cluster deployment
  val api = RedisApi.Keyed.Async.StringTyped(client)

  // Appropriate master node is automatically chosen for execution based on hash of the key and current cluster slot mapping
  api.get("key").onComplete {
    case Success(Opt(value)) => println(s"Got value $value")
    case Success(Opt.Empty) => println(s"Got no value")
    case Failure(t) => t.printStackTrace()
  }
}
