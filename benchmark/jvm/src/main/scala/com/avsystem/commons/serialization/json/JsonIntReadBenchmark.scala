package com.avsystem.commons
package serialization.json

import org.openjdk.jmh.annotations.*

/** End-to-end Long reading, parsed straight from the reader buffer (no per-number substring allocation). Inspect
  * `gc.alloc.rate.norm` under `-prof gc`.
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

  @Benchmark
  def readLongs: List[Long] =
    JsonStringInput.read[List[Long]](json)
}
