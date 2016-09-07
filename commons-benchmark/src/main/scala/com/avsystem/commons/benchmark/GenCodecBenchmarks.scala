package com.avsystem.commons
package benchmark

import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization._
import org.openjdk.jmh.annotations._

@Warmup(iterations = 5)
@Measurement(iterations = 20)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
class GenCodecBenchmarks {
  @Benchmark
  def testCaseClassCodec(): Unit = {
    GenCodec.read[Something](DummyInput)
  }
}

case class Something(int: Int, str: String)
object Something {
  implicit val codec: GenCodec[Something] = GenCodec.materialize
}

object DummyInput extends Input {
  private def ignored = throw new ReadFailure("don't care")

  def inputType = InputType.Object
  def readBinary() = ignored
  def readLong() = ignored
  def readNull() = ignored
  def readObject() = new ObjectInput {
    private val it = Iterator(
      new SimpleValueFieldInput("int", 42),
      new SimpleValueFieldInput("str", "lol")
    )
    def nextField() = it.next()
    def hasNext = it.hasNext
  }
  def readInt() = ignored
  def readString() = ignored
  def readList() = ignored
  def readBoolean() = ignored
  def readDouble() = ignored
  def skip() = ()
}
