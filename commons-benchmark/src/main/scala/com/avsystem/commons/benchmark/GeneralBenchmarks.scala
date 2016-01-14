package com.avsystem.commons
package benchmark

import org.openjdk.jmh.annotations._

@Warmup(iterations = 5)
@Measurement(iterations = 20)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
class GeneralBenchmarks {
  @Benchmark
  def foreachTest(): Unit = {
    var bin: Int = 0
    (1 to 100).foreach { i =>
      bin = i
    }
  }

  @Benchmark
  def whileTest(): Unit = {
    var bin: Int = 0
    var i = 0
    while (i < 100) {
      bin = i
      i += 1
    }
  }
}
