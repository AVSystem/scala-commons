package com.avsystem.commons
package rpc.akka

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, OutputTimeUnit, Scope, Setup, State, TearDown, Warmup}

/**
  * @author Wojciech Milewski
  */
@Warmup(iterations = 7)
@Measurement(iterations = 20)
@Fork(1)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
class DummyBenchmark {
  import DummyBenchmark._

  println("Benchmark CONSTRUCTOR")

  @Benchmark
  def benchmark(benchmarkState: BenchmarkState, threadState: ThreadState): Unit = {

  }


  @Benchmark
  def benchmark2(benchmarkState: BenchmarkState, threadState: ThreadState): Unit = {

  }
}

object DummyBenchmark {
  println("COMPANION")
  val counter = new AtomicInteger(0)

  @State(Scope.Benchmark)
  class BenchmarkState {
    println("BenchmarkState CONSTRUCTOR")

    @Setup
    def setup(): Unit = {
      println(s"BenchmarkState SETUP: ${counter.getAndIncrement()}")

    }

    @TearDown()
    def tearDown(): Unit = {
      println(s"BenchmarkState TEARDOWN: ${counter.getAndIncrement()}")

    }
  }

  @State(Scope.Thread)
  class ThreadState {
    println("ThreadState CONSTRUCTOR")

    @Setup
    def setup(): Unit = {
      println(s"ThreadState SETUP ${counter.getAndIncrement()}")
    }

    @TearDown()
    def tearDown(): Unit = {
      println(s"ThreadState TEARDOWN: ${counter.getAndIncrement()}")

    }
  }
}