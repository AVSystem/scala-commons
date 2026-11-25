package com.avsystem.commons
package ser

import com.avsystem.commons.serialization.json.{JsonIntegerReading, JsonOptions, JsonReader, JsonStringInput}
import org.openjdk.jmh.annotations.*
import org.openjdk.jmh.infra.Blackhole

/** Reading every element of a 10k-element JSON array with `readInt` / `readLong`, under [[JsonIntegerReading.Default]]
  * (non-plain numbers such as `2.0` or `5e3` go through `Double` / `BigDecimal` rounding) and under
  * [[JsonIntegerReading.Exact]] (they are read exactly). Plain integers show the cost of the option on the common path;
  * decimal and exponent forms show the cost of the non-plain path. Every element is a whole number in range, so both
  * modes read all of them successfully. Run with `-prof gc` and compare `gc.alloc.rate.norm`.
  */
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 1)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Benchmark)
class JsonIntegerReadBenchmark {
  private final val Size = 10000

  private val defaultOptions: JsonOptions = JsonOptions.Default
  private val exactOptions: JsonOptions = JsonOptions(integerReading = JsonIntegerReading.Exact)

  private val plainInts: String =
    Iterator.tabulate(Size)(i => (i * 7919 - 30000000).toString).mkString("[", ",", "]")

  private val decimalInts: String =
    Iterator.tabulate(Size)(i => s"${i * 31}.0").mkString("[", ",", "]")

  private val exponentInts: String =
    Iterator.tabulate(Size)(i => s"${i % 1000}.${i % 7}e3").mkString("[", ",", "]")

  private val decimalLongs: String =
    Iterator.tabulate(Size)(i => s"${i.toLong * 1000003L}.000").mkString("[", ",", "]")

  private def readInts(json: String, options: JsonOptions, bh: Blackhole): Unit = {
    val li = new JsonStringInput(new JsonReader(json), options).readList()
    while (li.hasNext) {
      bh.consume(li.nextElement().readInt())
    }
  }

  private def readLongs(json: String, options: JsonOptions, bh: Blackhole): Unit = {
    val li = new JsonStringInput(new JsonReader(json), options).readList()
    while (li.hasNext) {
      bh.consume(li.nextElement().readLong())
    }
  }

  @Benchmark
  def plainIntsDefault(bh: Blackhole): Unit = readInts(plainInts, defaultOptions, bh)

  @Benchmark
  def plainIntsExact(bh: Blackhole): Unit = readInts(plainInts, exactOptions, bh)

  @Benchmark
  def decimalIntsDefault(bh: Blackhole): Unit = readInts(decimalInts, defaultOptions, bh)

  @Benchmark
  def decimalIntsExact(bh: Blackhole): Unit = readInts(decimalInts, exactOptions, bh)

  @Benchmark
  def exponentIntsDefault(bh: Blackhole): Unit = readInts(exponentInts, defaultOptions, bh)

  @Benchmark
  def exponentIntsExact(bh: Blackhole): Unit = readInts(exponentInts, exactOptions, bh)

  @Benchmark
  def decimalLongsDefault(bh: Blackhole): Unit = readLongs(decimalLongs, defaultOptions, bh)

  @Benchmark
  def decimalLongsExact(bh: Blackhole): Unit = readLongs(decimalLongs, exactOptions, bh)
}
