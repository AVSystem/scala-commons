package com.avsystem.commons
package core

import org.openjdk.jmh.annotations._

@Warmup(iterations = 5)
@Measurement(iterations = 20)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
class OptBenchmarks {
  def doSomething(str: String): Unit = ()

  @Benchmark
  def testOption: Double =
    Option("lol").map(_.length).filter(_ < 10).map(_.toDouble).getOrElse(0)

  @Benchmark
  def testOpt: Double =
    Opt("lol").map(_.length).filter(_ < 10).map(_.toDouble).getOrElse(0)

  @Benchmark
  def testNOpt: Double =
    NOpt("lol").map(_.length).filter(_ < 10).map(_.toDouble).getOrElse(0)
}
