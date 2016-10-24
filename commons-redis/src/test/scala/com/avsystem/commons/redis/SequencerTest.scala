package com.avsystem.commons
package redis

import com.avsystem.commons.misc.Opt

object SequencerTest {

  import RedisApi.Batches.StringTyped._

  val tupleBatch: RedisBatch[(Opt[String], Long)] =
    (get("key1"), incr("key2")).sequence

  val seqBatch: RedisBatch[Seq[Opt[String]]] =
    (1 to 10).map(i => get(s"key$i")).sequence

  val tupleCollectionBatch: RedisBatch[Seq[(Opt[String], Long)]] =
    (1 to 10).map(i => (get(s"stringKey$i"), incr(s"numberKey$i"))).sequence

  // tuple of batches -> single batch of a tuple
  val tupleBatch2: RedisBatch[(Opt[String], Long)] =
    RedisBatch.sequence(get("key1"), incr("key2"))

  // collection of batches -> single batch of a collection
  val seqBatch2: RedisBatch[Seq[Opt[String]]] =
    RedisBatch.sequence((1 to 10).map(i => get(s"key$i")))

  // collection of tuples of batches -> single batch of collection of tuples
  val tupleCollectionBatch2: RedisBatch[Seq[(Opt[String], Long)]] =
    RedisBatch.sequence((1 to 10).map(i => (get(s"stringKey$i"), incr(s"numberKey$i"))))
}
