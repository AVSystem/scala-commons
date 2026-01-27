package com.avsystem.commons.misc

import scala.compiletime.ops.int.S

trait TupleOps {
  extension [Tup <: Tuple](tup: Tup) {
    inline def foreach(f: [t] => t => Unit): Unit = tup.map[[X] =>> Unit](f)

    inline def indices: Indices[Tup] =
      Tuple.fromArray(Array.range(0, tup.size)).asInstanceOf[Indices[Tup]]
  }

  type Indices[Tup <: Tuple] <: Tuple = Tup match {
    case EmptyTuple => EmptyTuple
    case h *: t => 0 *: IndicesAux[t, 1]
  }

  type IndicesAux[Tup <: Tuple, N <: Int] <: Tuple = Tup match {
    case EmptyTuple => EmptyTuple
    case h *: t => N *: IndicesAux[t, S[N]]
  }
}
