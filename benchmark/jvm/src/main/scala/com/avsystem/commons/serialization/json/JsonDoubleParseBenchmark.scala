package com.avsystem.commons
package serialization.json

import org.openjdk.jmh.annotations.*

/** Compares Double parsing: java.lang.Double.parseDouble vs the Eisel-Lemire fast parser used by JsonStringInput. */
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 2)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Thread)
class JsonDoubleParseBenchmark {
  // 128 double strings with a spread of magnitudes/shapes (same distribution as the write-side BigDoubleMap).
  private[this] val strings: Array[String] = Array.tabulate(128) { i =>
    (((i + 1) * 0.123456789) * (if (i % 2 == 0) 1 else -1) * math.pow(10, (i % 7) - 3)).toString
  }

  @Benchmark
  def parseStandard: Double = {
    var sum = 0.0
    var i = 0
    while (i < strings.length) { sum += java.lang.Double.parseDouble(strings(i)); i += 1 }
    sum
  }

  @Benchmark
  def parseEiselLemire: Double = {
    var sum = 0.0
    var i = 0
    while (i < strings.length) { sum += EiselLemireDouble.parse(strings(i)); i += 1 }
    sum
  }
}
