package com.avsystem.commons
package serialization.json

import org.openjdk.jmh.annotations.*

/** End-to-end Long reading: Standard (Long.parseLong on a per-number substring) vs Fast (parsed straight from the
  * reader buffer). Compare `gc.alloc.rate.norm` under `-prof gc` for the substring-allocation saving.
  */
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 2)
@Fork(2)
@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Thread)
class JsonIntReadBenchmark {
  private[this] val json: String = (0 until 512)
    .map(i => (i.toLong * 2654435761L - 1234567890123L).toString)
    .mkString("[", ",", "]")

  private[this] val fast = JsonOptions(numberCodec = JsonNumberCodec.Fast)

  @Benchmark
  def readStandard: List[Long] =
    JsonStringInput.read[List[Long]](json, JsonOptions.Default)

  @Benchmark
  def readFast: List[Long] =
    JsonStringInput.read[List[Long]](json, fast)
}
