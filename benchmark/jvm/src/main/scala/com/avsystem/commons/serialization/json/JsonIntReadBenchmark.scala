package com.avsystem.commons
package serialization.json

import org.openjdk.jmh.annotations.*

/** End-to-end Long reading. `Int`/`Long` reads are unchanged from the released code; this benchmark exists to confirm
  * they stay at parity (throughput and `gc.alloc.rate.norm` under `-prof gc`) after the `Double`/`Float` changes.
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
