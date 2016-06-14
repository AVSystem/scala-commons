package com.avsystem.commons
package rpc.akka.serialization

import akka.util.ByteString
import com.avsystem.commons.serialization.GenCodec
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, Scope, State, Warmup}
import org.openjdk.jmh.infra.Blackhole

/**
  * @author Wojciech Milewski
  */
@Warmup(iterations = 5)
@Measurement(iterations = 20)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Thread)
class GenCodecBenchmark {

  val something = Something(42, Nested(4 :: 8 :: 15 :: 16 :: 23 :: 42 :: Nil, 0), "lol")
  val array: Array[Byte] = {
    val output = new ByteStringOutput()
    GenCodec.write[Something](output, something)
    output.result.toArray
  }

  @Benchmark
  def conversionTest(): Unit = {
    val output = new ByteStringOutput
    GenCodec.write[Something](output, something)
    val array: Array[Byte] = output.result.toArray

    val input = new ByteStringInput(ByteString(array))
    val result = GenCodec.read[Something](input)

    assert(result == something)
  }

  @Benchmark
  def writeTest(b: Blackhole): Unit = {
    val output = new ByteStringOutput()
    GenCodec.write[Something](output, something)
    b.consume(output.result)
  }

  @Benchmark
  def readTest(): Unit = {
    val input = new ByteStringInput(ByteString(array))
    val result = GenCodec.read[Something](input)

    assert(result == something)
  }

}
