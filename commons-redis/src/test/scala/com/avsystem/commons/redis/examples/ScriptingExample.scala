package com.avsystem.commons
package redis.examples

import akka.actor.ActorSystem
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.commands.{RedisScript, ReplyDecoders}
import com.avsystem.commons.redis.config.{ClusterConfig, NodeConfig}
import com.avsystem.commons.redis.protocol.{BulkStringMsg, NullBulkStringMsg}

/**
  * Example which shows how to execute LUA scripts.
  */
object ScriptingExample extends App {
  implicit val actorSystem = ActorSystem()

  val client = new RedisNodeClient
  val api = RedisApi.Node.Async.StringTyped(client)

  // The cleanest way to execute a LUA script is to first define it. Because format of script result is unknown in
  // general and depends on the contents of the script itself, you must provide your own decoder for the response.
  // Decoders can be written manually (as PartialFunction[ValidRedisMsg, A] where A is the type of decoded result)
  // or built using utilities that you can find in ReplyDecoders object.

  // Script with manually defined decoder.
  val script1: RedisScript[Opt[String]] =
    RedisScript("return redis.call('get', KEYS[1])") {
      case BulkStringMsg(data) => RedisDataCodec.read[String](data).opt
      case NullBulkStringMsg => Opt.Empty
    }

  // Script with decoder created using ReplyDecoders
  val script2: RedisScript[Opt[String]] =
    RedisScript("return redis.call('get', KEYS[1])")(ReplyDecoders.nullBulkOr[String])

  // The safest way to execute script with Async API is to use evalshaOrEval which first tries to execute the script
  // using EVALSHA and falls back to EVAL if the script is not yet loaded into Redis. This way we avoid sending
  // the entire script to Redis every time but at the same time we don't have to preload it using SCRIPT LOAD
  def runScript1: Future[Opt[String]] =
  api.evalshaOrEval(script1, Seq("key"), Seq())

  // If you want to preload scripts before evaluating them, you can use `initOp` setting of `NodeConfig`
  // This is especially useful when using `RedisClusterClient` where nodes can appear and disappear at any time
  val nodeConfig = NodeConfig(initOp = RedisApi.Batches.StringTyped.scriptLoad(script1).operation)
  val clusterConfig = ClusterConfig(nodeConfigs = _ => nodeConfig)
  val clusterClient = new RedisClusterClient(config = clusterConfig)
  val clusterApi = RedisApi.Keyed.Async.StringTyped(clusterClient)

  // No need to worry about script loading now (at least as long as Redis is not restarted...)
  def runScript2: Future[Opt[String]] =
    clusterApi.evalsha(script1, Seq("key"), Seq())
}
