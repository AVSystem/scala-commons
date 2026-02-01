package com.avsystem.commons
package core

import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

@Warmup(iterations = 5)
@Measurement(iterations = 20)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
class LoopBenchmark {

  import LoopBenchmark._

  @Benchmark
  def rangeForeachTest(blackhole: Blackhole): Unit = {
    val x = 42
    var bin: Int = 0
    (1 to 100).foreach { i =>
      bin = i + x
    }
    blackhole.consume(bin)
  }

  @Benchmark
  def foreachTest(blackhole: Blackhole): Unit = {
    val x = 42
    var bin: Int = 0
    collection.foreach { i =>
      bin = i + x
    }
    blackhole.consume(bin)
  }

  @Benchmark
  def iteratorForeachTest(blackhole: Blackhole): Unit = {
    val x = 42
    var bin: Int = 0
    collection.iterator.foreach { i =>
      bin = i + x
    }
    blackhole.consume(bin)
  }

  @Benchmark
  def iteratorTest(blackhole: Blackhole): Unit = {
    val x = 42
    var bin: Int = 0
    val it = collection.iterator
    while (it.hasNext) {
      bin = it.next() + x
    }
    blackhole.consume(bin)
  }

  @Benchmark
  def whileTest(blackhole: Blackhole): Unit = {
    val x = 42
    var bin: Int = 0
    var l = collection
    while (l.nonEmpty) {
      bin = l.head + x
      l = l.tail
    }
    blackhole.consume(bin)
  }
}

object LoopBenchmark {
  val collection: List[Int] = (1 to 100).toList
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
