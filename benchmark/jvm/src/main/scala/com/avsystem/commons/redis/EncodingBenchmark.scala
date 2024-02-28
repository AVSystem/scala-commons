package com.avsystem.commons
package redis

import org.apache.pekko.util.{ByteString, ByteStringBuilder}
import com.avsystem.commons.redis.protocol.{ArrayMsg, BulkStringMsg, IntegerMsg, NullBulkStringMsg, RedisMsg, SimpleStringMsg}
import org.openjdk.jmh.annotations._

@Warmup(iterations = 5)
@Measurement(iterations = 20)
@Fork(1)
@Threads(1)
@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Benchmark)
class EncodingBenchmark {
  private val bsb = new ByteStringBuilder

  final val msg = ArrayMsg(IndexedSeq(
    IntegerMsg(12345342323L),
    IntegerMsg(1231),
//    BulkStringMsg(ByteString("jkalsjdkflajsdkfhlkasd")),
//    SimpleStringMsg(ByteString("sjakdlfjaksdhfjakshd")),
    NullBulkStringMsg
  ))

  @Benchmark
  def encodeBenchmark() = {
    RedisMsg.encode(msg, bsb)
    bsb.clear()
    bsb
  }
}

object EncodingBenchmark {
  def main(args: Array[String]): Unit = {
    val b = new EncodingBenchmark
    while (true) {
      b.encodeBenchmark()
    }
  }
}