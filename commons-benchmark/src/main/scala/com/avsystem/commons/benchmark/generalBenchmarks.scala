package com.avsystem.commons
package benchmark

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@Warmup(iterations = 5)
@Measurement(iterations = 20)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
class LoopBenchmark {
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

class Box(var n: Int) {
  def copy(n: Int = this.n) =
    new Box(n)
}

@Warmup(iterations = 5)
@Measurement(iterations = 20)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
class AllocationBenchmark {
  @Benchmark
  def copyTest(blackhole: Blackhole): Unit = {
    var box = new Box(0)
    var i = 0
    while (i < 1000) {
      box = box.copy(n = i)
      i += 1
    }
    blackhole.consume(box)
  }

  @Benchmark
  def mutateTest(blackhole: Blackhole): Unit = {
    val box = new Box(0)
    var i = 0
    while (i < 1000) {
      box.n = i
      i += 1
    }
    blackhole.consume(box)
  }
}
