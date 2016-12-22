package com.avsystem.commons
package redis

import com.avsystem.commons.redis.commands.SetslotCmd.{Importing, Migrating, Node}
import com.avsystem.commons.redis.config.ClusterConfig
import org.scalatest.Suite

import scala.concurrent.Future

/**
  * Author: ghik
  * Created: 14/04/16.
  */
trait UsesRedisClusterClient extends UsesClusterServers with UsesActorSystem { this: Suite =>
  def clusterConfig: ClusterConfig = ClusterConfig()

  var redisClient: RedisClusterClient = _

  protected def migrateSlot(slot: Int, targetNodeSlot: Int, incomplete: Boolean = false): Future[Unit] =
    redisClient.initialized.flatMapNow { client =>
      val state = client.currentState
      val sourceClient = state.clientForSlot(slot)
      val targetClient = state.clientForSlot(targetNodeSlot)
      if (sourceClient != targetClient) {
        val sourceApi = RedisApi.Node.Async.BinaryTyped(sourceClient)
        val targetApi = RedisApi.Node.Async.BinaryTyped(targetClient)
        val sourceIdFut = sourceApi.clusterMyid
        val targetIdFut = targetApi.clusterMyid
        val keysToMigrateFut = sourceApi.clusterGetkeysinslot(slot, Int.MaxValue)
        for {
          sourceId <- sourceIdFut
          targetId <- targetIdFut
          _ <- sourceApi.clusterSetslot(slot, Migrating(targetId))
          _ <- targetApi.clusterSetslot(slot, Importing(sourceId))
          keys <- keysToMigrateFut
          _ <- if (keys.nonEmpty) sourceApi.migrate(keys, targetClient.address, 0, Int.MaxValue) else Future.successful(())
          _ <- {
            if (incomplete) Future.successful(())
            else Future.traverse(List(sourceApi, targetApi))(_.clusterSetslot(slot, Node(targetId)))
          }
        } yield ()
      } else Future.successful(())
    }

  override protected def beforeAll() = {
    super.beforeAll()
    redisClient = new RedisClusterClient(addresses.take(1), clusterConfig)
  }

  override protected def afterAll() = {
    redisClient.close()
    super.afterAll()
  }
}
