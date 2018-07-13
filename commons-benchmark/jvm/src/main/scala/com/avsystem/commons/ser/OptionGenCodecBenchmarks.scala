package com.avsystem.commons
package ser

import com.avsystem.commons.serialization._
import com.avsystem.commons.serialization.json.JsonStringOutput
import org.openjdk.jmh.annotations._

@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 2)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
class OptionGenCodecBenchmarks {
  @Benchmark
  def someWritingBenchmark(): String = JsonStringOutput.write(OptionGenCodecBenchmarks.somes)

  @Benchmark
  def noneWritingBenchmark(): String = JsonStringOutput.write(OptionGenCodecBenchmarks.nones)

  @Benchmark
  def cleanSomeWritingBenchmark(): String = {
    implicit val cleanCodec: GenCodec[Option[String]] = OptionGenCodecBenchmarks.cleanOptionCodec[String]
    JsonStringOutput.write(OptionGenCodecBenchmarks.somes)
  }

  @Benchmark
  def cleanNoneWritingBenchmark(): String = {
    implicit val cleanCodec: GenCodec[Option[String]] = OptionGenCodecBenchmarks.cleanOptionCodec[String]
    JsonStringOutput.write(OptionGenCodecBenchmarks.nones)
  }
}

object OptionGenCodecBenchmarks {
  implicit def cleanOptionCodec[T: GenCodec]: GenCodec[Option[T]] =
    GenCodec.create[Option[T]](
      i => if (i.isNull) {
        i.readNull()
        None
      } else Some(GenCodec.read[T](i)),
      (o, vo) => vo match {
        case Some(v) => GenCodec.write[T](o, v)
        case None => o.writeNull()
      }
    )

  val somes: Seq[Option[String]] = MArrayBuffer.tabulate(1000)(i => Some(i.toString))
  val nones: Seq[Option[String]] = MArrayBuffer.fill(1000)(None)
}
