package com.avsystem.commons
package serialization

import scala.Tuple.Map
import scala.compiletime.ops.int.S

trait TupleGenCodecs { this: GenCodec.type =>
  inline given [Tup <: Tuple] => GenCodec[Tup] = mkTupleCodec(compiletime.summonAll[Tuple.Map[Tup, GenCodec]])

  private def mkTupleCodec[Tup <: Tuple](elementCodecs: Tuple.Map[Tup, GenCodec]): GenCodec[Tup] = new ListCodec[Tup] {
    override val nullable = true
    override def readList(input: ListInput): Tup =
      elementCodecs
        .map[[X] =>> Any]([C] => (codec: C) => codec.asInstanceOf[GenCodec[?]].read(input.nextElement()))
        .asInstanceOf[Tup]
    override def writeList(output: ListOutput, value: Tup): Unit = {
      output.declareSize(elementCodecs.size)

      elementCodecs.indices
        .foreach([I] =>
          (i: I) =>
            elementCodecs(i.asInstanceOf[Int])
              .asInstanceOf[GenCodec[Any]]
              .write(output.writeElement(), value(i.asInstanceOf[Int])),
        )
    }
  }
}
