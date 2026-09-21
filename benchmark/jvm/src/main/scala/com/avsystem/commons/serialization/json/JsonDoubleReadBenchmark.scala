package com.avsystem.commons
package serialization.json

import org.openjdk.jmh.annotations.*

/** Double reading. `parseDoubles` isolates the EiselLemire parse over each number's text; `readListEndToEnd` is the full
  * JsonStringInput deserialization for context.
  */
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 2)
@Fork(2)
@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Thread)
class JsonDoubleReadBenchmark {
  private[this] val count = 512
  private[this] val starts = new Array[Int](count)
  private[this] val ends = new Array[Int](count)
  // A JSON array of `count` doubles; record each number's [start, end) range as we build it.
  private[this] val json: String = {
    val sb = new java.lang.StringBuilder("[")
    var i = 0
    while (i < count) {
      val d = ((i + 1) * 0.123456789) * (if (i % 2 == 0) 1 else -1) * math.pow(10, (i % 13) - 6)
      val s = d.toString
      starts(i) = sb.length
      sb.append(s)
      ends(i) = sb.length
      if (i < count - 1) sb.append(',')
      i += 1
    }
    sb.append(']')
    sb.toString
  }

  @Benchmark
  def parseDoubles: Double = {
    var sum = 0.0
    var i = 0
    while (i < count) { sum += EiselLemireDouble.parse(json.substring(starts(i), ends(i))); i += 1 }
    sum
  }

  @Benchmark
  def readListEndToEnd: List[Double] =
    JsonStringInput.read[List[Double]](json)
}
