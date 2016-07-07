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
class GenCodecLinearBenchmark {

  val something = Something(42, Nested(4 :: 8 :: 15 :: 16 :: 23 :: 42 :: Nil, 0), "lol")
  val array: Array[Byte] = {
    val output = new ByteStringLinearOutput(ByteString.newBuilder)
    GenCodec.write[Something](output, something)
    output.result.toArray
  }

  @Benchmark
  def conversionTest(): Unit = {
    val output = new ByteStringLinearOutput(ByteString.newBuilder)
    GenCodec.write[Something](output, something)
    val array: Array[Byte] = output.result.toArray

    val input = new ByteStringLinearInput(ByteString(array))
    val result = GenCodec.read[Something](input)

    assert(result == something)
  }

  @Benchmark
  def writeTest(b: Blackhole): Unit = {
    val output = new ByteStringLinearOutput(ByteString.newBuilder)
    GenCodec.write[Something](output, something)
    b.consume(output.result)
  }

  @Benchmark
  def readTest(): Unit = {
    val input = new ByteStringLinearInput(ByteString(array))
    val result = GenCodec.read[Something](input)

    assert(result == something)
  }

}

object GenCodecLinearBenchmark {

  def write(): Unit = {
    val something = Something(42, Nested(4 :: 8 :: 15 :: 16 :: 23 :: 42 :: Nil, 0), "lol")

    1.to(1000000).foreach { _ =>
      val output = new ByteStringLinearOutput(ByteString.newBuilder)
      GenCodec.write[Something](output, something)
    }

  }

  def read(): Unit = {
    val something = Something(42, Nested(4 :: 8 :: 15 :: 16 :: 23 :: 42 :: Nil, 0), "lol")
    val array: Array[Byte] = {
      val output = new ByteStringLinearOutput(ByteString.newBuilder)
      GenCodec.write[Something](output, something)
      output.result.toArray
    }

    1.to(1000000).foreach { _ =>
      val input = new ByteStringLinearInput(ByteString(array))
      val result = GenCodec.read[Something](input)
    }

  }

  def main(args: Array[String]): Unit = {
    read()
  }
}