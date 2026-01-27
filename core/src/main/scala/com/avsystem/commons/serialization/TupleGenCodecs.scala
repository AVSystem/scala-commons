package com.avsystem.commons.serialization

trait TupleGenCodecs { this: GenCodec.type =>
  inline given [Tup <: Tuple] => GenCodec[Tup] =
    mkTupleCodec(compiletime.summonAll[Tuple.Map[Tup, GenCodec]].toList.asInstanceOf[List[GenCodec[?]]]*)
  private def mkTupleCodec[T](elementCodecs: GenCodec[?]*): GenCodec[T] = ???
}
