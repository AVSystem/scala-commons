package com.avsystem.commons
package core

import org.openjdk.jmh.annotations._

@Warmup(iterations = 5)
@Measurement(iterations = 20)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
class OptBenchmarks {
  def doSomething(str: String): Unit = ()

  def takeOpt(os: Opt[String]): Opt[String] =
    os.filter(_.length < 10).map(_.toDouble).map(_.toString)

  def takeOption(os: Option[String]): Option[String] =
    os.filter(_.length < 10).map(_.toDouble).map(_.toString)

  @Benchmark
  def testOption: String = takeOption(Option("1234.56")).getOrElse("")

  @Benchmark
  def testOpt: String = takeOpt(Opt("1234.56")).getOrElse("")
}
