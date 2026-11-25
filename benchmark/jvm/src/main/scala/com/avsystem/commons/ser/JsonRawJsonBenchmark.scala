package com.avsystem.commons
package ser

import com.avsystem.commons.serialization.json.{JsonReader, JsonStringInput, JsonType, RawJson}
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

/** Reading every element of a 10k-element JSON array as raw JSON (`readMetadata(JsonType)` + `readCustom(RawJson)`,
  * as a codec that inspects a value's text before deciding how to read it would), compared with reading the same
  * integers with `readInt`. Run with `-prof gc` and compare `gc.alloc.rate.norm`.
  */
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Benchmark)
class JsonRawJsonBenchmark {
  private final val Size = 10000

  private val ints: String =
    Iterator.tabulate(Size)(i => (i * 7919 - Size * 3000).toString).mkString("[", ",", "]")

  private val decimals: String =
    Iterator.tabulate(Size)(i => s"${i * 31}.${i % 997}e-3").mkString("[", ",", "]")

  private val literals: String =
    Iterator.tabulate(Size)(i => if (i % 3 == 0) "true" else if (i % 3 == 1) "false" else "null").mkString("[", ",", "]")

  private def readRaw(json: String, bh: Blackhole): Unit = {
    val li = new JsonStringInput(new JsonReader(json)).readList()
    while (li.hasNext) {
      val in = li.nextElement()
      bh.consume(in.readMetadata(JsonType).get)
      bh.consume(in.readCustom(RawJson).get)
    }
  }

  @Benchmark
  def intsReadInt(bh: Blackhole): Unit = {
    val li = new JsonStringInput(new JsonReader(ints)).readList()
    while (li.hasNext) {
      bh.consume(li.nextElement().readInt())
    }
  }

  @Benchmark
  def intsRaw(bh: Blackhole): Unit = readRaw(ints, bh)

  @Benchmark
  def decimalsRaw(bh: Blackhole): Unit = readRaw(decimals, bh)

  @Benchmark
  def literalsRaw(bh: Blackhole): Unit = readRaw(literals, bh)
}
