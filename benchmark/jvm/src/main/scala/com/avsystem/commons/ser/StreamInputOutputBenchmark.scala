package com.avsystem.commons
package ser

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}

import com.avsystem.commons.serialization.{GenCodec, StreamInput, StreamOutput}
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Measurement, Mode, Scope, State, Warmup}
import org.openjdk.jmh.infra.Blackhole


case class Toplevel(int: Int, nested: Nested, str: String)
case class Nested(list: List[Int], int: Int)

object Toplevel {
  implicit val nestedCodec: GenCodec[Nested] = GenCodec.materialize[Nested]
  implicit val codec: GenCodec[Toplevel] = GenCodec.materialize[Toplevel]
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

    GenCodec.write(new StreamOutput(new DataOutputStream(os)), something)
    os.toByteArray
  }

  @Benchmark
  def testEncode(bh: Blackhole): Unit = {
    val os = new ByteArrayOutputStream(inputArray.length)
    val output = new StreamOutput(new DataOutputStream(os))
    GenCodec.write(output, something)
    bh.consume(os.toByteArray)
  }

  @Benchmark
  def testDecode(bh: Blackhole): Unit = {
    val is = new DataInputStream(new ByteArrayInputStream(inputArray))
    val input = new StreamInput(is)
    bh.consume(GenCodec.read[Toplevel](input))
  }

  @Benchmark
  def testEncodeRaw(bh: Blackhole): Unit = {
    val os = new ByteArrayOutputStream(inputArray.length)
    val output = new StreamOutput(new DataOutputStream(os))
    val toplevelOutput = output.writeObject()
    toplevelOutput.writeField("int").writeSimple().writeInt(35)
    val nestedOutput = toplevelOutput.writeField("nested").writeObject()
    val listOutput = nestedOutput.writeField("list").writeList()
    listOutput.writeElement().writeSimple().writeInt(121)
    listOutput.writeElement().writeSimple().writeInt(122)
    listOutput.writeElement().writeSimple().writeInt(123)
    listOutput.writeElement().writeSimple().writeInt(124)
    listOutput.writeElement().writeSimple().writeInt(125)
    listOutput.writeElement().writeSimple().writeInt(126)
    listOutput.finish()
    nestedOutput.writeField("int").writeSimple().writeInt(53)
    nestedOutput.finish()
    toplevelOutput.writeField("str").writeSimple().writeString("lol")
    toplevelOutput.finish()
    bh.consume(os.toByteArray)
  }

  @Benchmark
  def testDecodeRaw(bh: Blackhole): Unit = {
    val is = new DataInputStream(new ByteArrayInputStream(inputArray))
    val input = new StreamInput(is)
    val objInput = input.readObject()
    val intField = objInput.nextField().readSimple().readInt()
    val nestedInput = objInput.nextField().readObject()
    val listInput = nestedInput.nextField().readList()
    val listNested = List(
      listInput.nextElement().readSimple().readInt(),
      listInput.nextElement().readSimple().readInt(),
      listInput.nextElement().readSimple().readInt(),
      listInput.nextElement().readSimple().readInt(),
      listInput.nextElement().readSimple().readInt(),
      listInput.nextElement().readSimple().readInt()
    )
    listInput.hasNext
    val intNested = nestedInput.nextField().readSimple().readInt()
    nestedInput.hasNext
    val strField = objInput.nextField().readSimple().readString()
    objInput.hasNext
    bh.consume(Toplevel(intField, Nested(listNested, intNested), strField))
  }
}
