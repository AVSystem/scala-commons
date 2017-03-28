package com.avsystem.commons
package redis.commands

import com.avsystem.commons.redis._


trait HashesApiSuite extends CommandsSuite {

  import RedisApi.Batches.StringTyped._

  apiTest("HDEL") {
    setup(hset("key", "field", "value"), hmset("key2", "field1" -> "value1", "field2" -> "value2"))
    hdel("key", Nil).assertEquals(0)
    hdel("???", "field").assertEquals(false)
    hdel("key", "???").assertEquals(false)
    hdel("key", "field").assertEquals(true)
    hdel("key2", "field1", "field2", "field3").assertEquals(2)
  }

  apiTest("HEXISTS") {
    setup(hset("key", "field", "value"))
    hexists("key", "???").assertEquals(false)
    hexists("key", "field").assertEquals(true)
  }

  apiTest("HGET") {
    setup(hset("key", "field", "value"))
    hget("???", "field").assertEquals(Opt.Empty)
    hget("key", "???").assertEquals(Opt.Empty)
    hget("key", "field").assertEquals("value".opt)
  }

  apiTest("HGETALL") {
    val fieldValues = Map("field1" -> "value1", "field2" -> "value2", "field3" -> "value3")
    setup(hmset("key", fieldValues))
    hgetall("???").assertEquals(Map.empty)
    hgetall("key").assertEquals(fieldValues)
  }

  apiTest("HINCRBY") {
    hincrby("key", "field", 3).assertEquals(3)
    hincrby("key", "field", -2).assertEquals(1)
  }

  apiTest("HINCRBYFLOAT") {
    hincrbyfloat("key", "field", 3.14).assertEquals(3.14)
    hincrbyfloat("key", "field", -2.0).assertEquals(1.14)
  }

  apiTest("HKEYS") {
    val fieldValues = Map("field1" -> "value1", "field2" -> "value2", "field3" -> "value3")
    setup(hmset("key", fieldValues))
    hkeys("???").assertEquals(Set.empty)
    hkeys("key").assertEquals(fieldValues.keySet)
  }

  apiTest("HLEN") {
    val fieldValues = Map("field1" -> "value1", "field2" -> "value2", "field3" -> "value3")
    setup(hmset("key", fieldValues))
    hlen("???").assertEquals(0)
    hlen("key").assertEquals(3)
  }

  apiTest("HMGET") {
    setup(hset("key", "field", "value"))
    hmget("key", Nil).assertEquals(Seq.empty)
    hmget("???", "field", "field2").assertEquals(Seq(Opt.Empty, Opt.Empty))
    hmget("key", "field", "field2").assertEquals(Seq("value".opt, Opt.Empty))
  }

  apiTest("HMSET") {
    hmset("key", Nil).get
    hmset("key", "field" -> "value", "f2" -> "v2").get
  }

  apiTest("HSCAN") {
    val scanFields = (0 until 256).map(i => (s"toscan$i", s"value$i")).toMap
    setup(hmset("key", scanFields))
    hscan("???", Cursor.NoCursor).assertEquals((Cursor.NoCursor, Seq.empty))
    def hscanCollect(cursor: Cursor, acc: Map[String,String]): Future[Map[String,String]] =
      hscan("key", cursor, "toscan*", 4).exec.flatMapNow {
        case (Cursor.NoCursor, data) => Future.successful(acc ++ data.toMap)
        case (nextCursor, data) => hscanCollect(nextCursor, acc ++ data.toMap)
      }
    hscanCollect(Cursor.NoCursor, Map.empty).futureValue.toSet shouldEqual scanFields.toSet
  }

  apiTest("HSET") {
    hset("key", "field", "value").assertEquals(true)
    hset("key", "field", "value").assertEquals(false)
  }

  apiTest("HSETNX") {
    hsetnx("key", "field", "value").assertEquals(true)
    hsetnx("key", "field", "value").assertEquals(false)
  }

  apiTest("HSTRLEN") {
    setup(hset("key", "field", "value"))
    hstrlen("???", "field").assertEquals(0)
    hstrlen("key", "???").assertEquals(0)
    hstrlen("key", "field").assertEquals(5)
  }

  apiTest("HVALS") {
    val fieldValues = Map("field1" -> "value1", "field2" -> "value2", "field3" -> "value3")
    setup(hmset("key", fieldValues))
    hvals("???").assertEquals(Seq.empty)
    hvals("key").map(_.toSet).assertEquals(fieldValues.values.toSet)
  }
}
