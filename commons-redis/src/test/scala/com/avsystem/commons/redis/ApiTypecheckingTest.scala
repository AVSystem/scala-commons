package com.avsystem.commons
package redis

import com.avsystem.commons.redis.commands.Encoding

object ApiTypecheckingTest {

  locally {
    import RedisApi.Batches.StringTyped._

    val tupleBatch: RedisBatch[(Opt[String], Long)] =
      (get("key1"), incr("key2")).sequence

    val seqBatch: RedisBatch[Seq[Opt[String]]] =
      (1 to 10).map(i => get(s"key$i")).sequence

    val tupleCollectionBatch: RedisBatch[Seq[(Opt[String], Long)]] =
      (1 to 10).map(i => (get(s"stringKey$i"), incr(s"numberKey$i"))).sequence
  }


  locally {
    import RedisApi.Batches.StringTyped._

    // tuple of batches -> single batch of a tuple
    val tupleBatch: RedisBatch[(Opt[String], Long)] =
      RedisBatch.sequence(get("key1"), incr("key2"))

    // collection of batches -> single batch of a collection
    val seqBatch: RedisBatch[Seq[Opt[String]]] =
      RedisBatch.sequence((1 to 10).map(i => get(s"key$i")))

    // collection of tuples of batches -> single batch of collection of tuples
    val tupleCollectionBatch2: RedisBatch[Seq[(Opt[String], Long)]] =
      RedisBatch.sequence((1 to 10).map(i => (get(s"stringKey$i"), incr(s"numberKey$i"))))
  }

  locally {
    import RedisApi.Batches.StringTyped._

    // collection of batches -> single batch of a collection
    val seqBatch: RedisBatch[Seq[Opt[String]]] =
      RedisBatch.traverse(1 to 10)(i => get(s"key$i"))

    // collection of tuples of batches -> single batch of collection of tuples
    val tupleCollectionBatch2: RedisBatch[Seq[(Opt[String], Long)]] =
      RedisBatch.traverse(1 to 10)(i => RedisBatch.sequence(get(s"stringKey$i"), incr(s"numberKey$i")))
  }

  locally {
    val api = RedisApi.Batches.StringTyped.valueType[Int]
    import api._
    val transactionOp: RedisOp[Unit] = for {
    // we're sending WATCH and GET commands in a single batch
      value <- watch("number") *> get("number").map(_.getOrElse(1))
      // SET command is wrapped in MULTI-EXEC block
      _ <- set("number", value * 3).transaction
    } yield ()
  }

  locally {
    val api = RedisApi.Batches.StringTyped.valueType[Encoding]
    api.set("lol", Encoding.HashTable)
  }

  locally {
    import RedisApi.Batches.StringTyped._
    val keys = (0 to 5).map(i => s"key$i")
    val sumBatch1: RedisBatch[Long] = RedisBatch.foldLeft(keys.map(scard), 0L)(_ + _)
    val sumBatch2: RedisBatch[Long] = RedisBatch.foldLeftMap(keys, 0L)(scard)(_ + _)
  }

}
