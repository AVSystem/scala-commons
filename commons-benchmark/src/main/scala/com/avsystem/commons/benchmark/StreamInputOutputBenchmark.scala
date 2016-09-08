package com.avsystem.commons
package benchmark

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream, OutputStream}

import com.avsystem.commons.serialization.{GenCodec, StreamInput, StreamOutput}
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, Scope, State, Warmup}
import org.openjdk.jmh.infra.Blackhole


case class Toplevel(int: Int, nested: Nested, str: String)
case class Nested(list: List[Int], int: Int)

object Toplevel {
  implicit val nestedCodec = GenCodec.materialize[Nested]
  implicit val codec = GenCodec.materialize[Toplevel]
}

private class DummyOutputStream(bh: Blackhole) extends OutputStream {
  override def write(b: Int): Unit = bh.consume(b)
}

@Warmup(iterations = 10)
@Measurement(iterations = 20)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Thread)
class StreamInputOutputBenchmark {

  val something = Toplevel(35, Nested(List(121, 122, 123, 124, 125, 126), 53), "lol")

  val inputArray: Array[Byte] = {
    val os = new ByteArrayOutputStream()

    GenCodec.autoWrite(new StreamOutput(new DataOutputStream(os)), something)
    os.toByteArray
  }

  @Benchmark
  def testEncode(bh: Blackhole): Unit = {
    val os = new DataOutputStream(new DummyOutputStream(bh))
    val output = new StreamOutput(os)
    GenCodec.autoWrite(output, something)
  }

  @Benchmark
  def testDecode(bh: Blackhole): Unit = {
    val is = new DataInputStream(new ByteArrayInputStream(inputArray))
    val input = new StreamInput(is)
    bh.consume(GenCodec.read[Toplevel](input))
  }

  @Benchmark
  def testEncodeRaw(bh: Blackhole): Unit = {
    val os = new DataOutputStream(new DummyOutputStream(bh))
    val output = new StreamOutput(os)
    val toplevelOutput = output.writeObject()
    toplevelOutput.writeField("int").writeInt(35)
    val nestedOutput = toplevelOutput.writeField("nested").writeObject()
    val listOutput = nestedOutput.writeField("list").writeList()
    listOutput.writeElement().writeInt(121)
    listOutput.writeElement().writeInt(122)
    listOutput.writeElement().writeInt(123)
    listOutput.writeElement().writeInt(124)
    listOutput.writeElement().writeInt(125)
    listOutput.writeElement().writeInt(126)
    listOutput.finish()
    nestedOutput.writeField("int").writeInt(53)
    nestedOutput.finish()
    toplevelOutput.writeField("str").writeString("lol")
    toplevelOutput.finish()
  }

  @Benchmark
  def testDecodeRaw(bh: Blackhole): Unit = {
    val is = new DataInputStream(new ByteArrayInputStream(inputArray))
    val input = new StreamInput(is)
    val objInput = input.readObject()
    bh.consume(objInput.nextField().readInt())
    val nestedInput = objInput.nextField().readObject()
    val listInput = nestedInput.nextField().readList()
    bh.consume(listInput.nextElement().readInt())
    bh.consume(listInput.nextElement().readInt())
    bh.consume(listInput.nextElement().readInt())
    bh.consume(listInput.nextElement().readInt())
    bh.consume(listInput.nextElement().readInt())
    bh.consume(listInput.nextElement().readInt())
    bh.consume(listInput.hasNext)
    bh.consume(nestedInput.nextField().readInt())
    bh.consume(nestedInput.hasNext)
    bh.consume(objInput.nextField().readString())
    bh.consume(objInput.hasNext)
  }
}

