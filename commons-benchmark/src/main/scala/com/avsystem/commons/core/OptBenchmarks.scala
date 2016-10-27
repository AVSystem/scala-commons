package com.avsystem.commons
package core

import com.avsystem.commons.misc.{NOpt, Opt}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@Warmup(iterations = 5)
@Measurement(iterations = 20)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
class OptBenchmarks {
  def doSomething(str: String): Unit = ()

  @Benchmark
  @OperationsPerInvocation(100)
  def testOption(bh: Blackhole): Unit = {
    var i = 0
    while (i < 100) {
      Option("lol").map(_.length).filter(_ < 10).map(_.toDouble) match {
        case Some(d) => bh.consume(d)
        case None =>
      }
      i += 1
    }
  }

  @Benchmark
  @OperationsPerInvocation(100)
  def testOpt(bh: Blackhole): Unit = {
    var i = 0
    while (i < 100) {
      Opt("lol").map(_.length).filter(_ < 10).map(_.toDouble) match {
        case Opt(d) => bh.consume(d)
        case Opt.Empty =>
      }
      i += 1
    }
  }

  @Benchmark
  @OperationsPerInvocation(100)
  def testNOpt(bh: Blackhole): Unit = {
    var i = 0
    while (i < 100) {
      NOpt("lol").map(_.length).filter(_ < 10).map(_.toDouble) match {
        case NOpt(d) => bh.consume(d)
        case NOpt.Empty =>
      }
      i += 1
    }
  }
}
