package com.avsystem.commons
package ser

import com.avsystem.commons.serialization._
import com.avsystem.commons.serialization.json.{JsonBinaryFormat, JsonOptions, JsonStringInput, JsonStringOutput}
import org.openjdk.jmh.annotations._

@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 2)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
class GenCodecBenchmarks {
  @Benchmark
  def someWriting: String = JsonStringOutput.write(GenCodecBenchmarks.somes)

  @Benchmark
  def noneWriting: String = JsonStringOutput.write(GenCodecBenchmarks.nones)

  @Benchmark
  def cleanSomeWriting: String = {
    implicit val cleanCodec: GenCodec[Option[String]] = GenCodecBenchmarks.cleanOptionCodec[String]
    JsonStringOutput.write(GenCodecBenchmarks.somes)
  }

  @Benchmark
  def cleanNoneWriting: String = {
    implicit val cleanCodec: GenCodec[Option[String]] = GenCodecBenchmarks.cleanOptionCodec[String]
    JsonStringOutput.write(GenCodecBenchmarks.nones)
  }

  @Benchmark
  def binaryReading: Array[Byte] =
    JsonStringInput.read[Array[Byte]](GenCodecBenchmarks.hex, GenCodecBenchmarks.options)
}

object GenCodecBenchmarks {
  given [T: GenCodec] => GenCodec[Option[T]] =
    GenCodec.create[Option[T]](
      i => if (i.readNull()) None else Some(GenCodec.read[T](i)),
      (o, vo) => vo match {
        case Some(v) => GenCodec.write[T](o, v)
        case None => o.writeNull()
      }
    )

  val somes: BSeq[Option[String]] = MArrayBuffer.tabulate(1000)(i => Some(i.toString))
  val nones: BSeq[Option[String]] = MArrayBuffer.fill(1000)(None)

  val options: JsonOptions = JsonOptions(binaryFormat = JsonBinaryFormat.HexString)
  val hex: String = JsonStringOutput.write(Array.tabulate[Byte](1024)(_.toByte), options)
}
