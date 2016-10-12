package com.avsystem.commons
package redis.commands

import com.avsystem.commons.misc.Opt
import com.avsystem.commons.redis._

import scala.concurrent.Future

trait HashesApiSuite extends CommandsSuite {

  import RedisApi.Batches.StringTyped._

  apiTest("HDEL") {
    setup(hset("key", "field", "value"), hmset("key2", "field1" -> "value1", "field2" -> "value2"))
    hdel("???", "field").assertEquals(0)
    hdel("key", "???").assertEquals(0)
    hdel("key", "field").assertEquals(1)
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
    val fieldValues = Seq("field1" -> "value1", "field2" -> "value2", "field3" -> "value3")
    setup(hmset("key", fieldValues: _*))
    hgetall("???").assertEquals(Seq.empty)
    hgetall("key").map(_.toSet).assertEquals(fieldValues.toSet)
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
    val fieldValues = Seq("field1" -> "value1", "field2" -> "value2", "field3" -> "value3")
    setup(hmset("key", fieldValues: _*))
    hkeys("???").assertEquals(Seq.empty)
    hkeys("key").map(_.toSet).assertEquals(fieldValues.map(_._1).toSet)
  }

  apiTest("HLEN") {
    val fieldValues = Seq("field1" -> "value1", "field2" -> "value2", "field3" -> "value3")
    setup(hmset("key", fieldValues: _*))
    hlen("???").assertEquals(0)
    hlen("key").assertEquals(3)
  }

  apiTest("HMGET") {
    setup(hset("key", "field", "value"))
    hmget("???", "field", "field2").assertEquals(Seq(Opt.Empty, Opt.Empty))
    hmget("key", "field", "field2").assertEquals(Seq("value".opt, Opt.Empty))
  }

  apiTest("HMSET") {
    hmset("key", "field" -> "value", "f2" -> "v2").get
  }

  apiTest("HSCAN") {
    val scanFields = (0 until 256).map(i => (s"toscan$i", s"value$i"))
    setup(hmset("key", scanFields: _*))
    hscan("???", Cursor.NoCursor).assertEquals((Cursor.NoCursor, Seq.empty))
    def hscanCollect(cursor: Cursor, acc: Seq[(String, String)]): Future[Seq[(String, String)]] =
      hscan("key", cursor, "toscan*", 4L).exec.flatMapNow {
        case (Cursor.NoCursor, data) => Future.successful(acc ++ data)
        case (nextCursor, data) => hscanCollect(nextCursor, acc ++ data)
      }
    hscanCollect(Cursor.NoCursor, Vector.empty).futureValue.toSet shouldEqual scanFields.toSet
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
    val fieldValues = Seq("field1" -> "value1", "field2" -> "value2", "field3" -> "value3")
    setup(hmset("key", fieldValues: _*))
    hvals("???").assertEquals(Seq.empty)
    hvals("key").map(_.toSet).assertEquals(fieldValues.map(_._2).toSet)
  }
}
