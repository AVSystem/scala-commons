package com.avsystem.commons
package benchmark

import com.avsystem.commons.misc.{NOpt, Opt}
import org.openjdk.jmh.annotations._

/**
  * Author: ghik
  * Created: 14/01/16.
  */
@Warmup(iterations = 5)
@Measurement(iterations = 20)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
class OptBenchmarks {
  def doSomething(str: String): Unit = ()

  @Benchmark
  @OperationsPerInvocation(100)
  def testOption(): Unit = {
    var i = 0
    while (i < 100) {
      Option("lol").map(_.length).filter(_ < 10).map(_.toDouble) match {
        case Some(_) =>
        case None =>
      }
      i += 1
    }
  }

  @Benchmark
  @OperationsPerInvocation(100)
  def testOpt(): Unit = {
    var i = 0
    while (i < 100) {
      Opt("lol").map(_.length).filter(_ < 10).map(_.toDouble) match {
        case Opt(_) =>
        case Opt.Empty =>
      }
      i += 1
    }
  }

  @Benchmark
  @OperationsPerInvocation(100)
  def testNOpt(): Unit = {
    var i = 0
    while (i < 100) {
      NOpt("lol").map(_.length).filter(_ < 10).map(_.toDouble) match {
        case NOpt(_) =>
        case NOpt.Empty =>
      }
      i += 1
    }
  }
}
