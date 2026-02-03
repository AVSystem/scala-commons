package com.avsystem.commons.misc

import scala.compiletime.ops.int.S
import scala.reflect.ClassTag
import scala.runtime.TupleXXL
import scala.runtime.Tuples.productToArray

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
  
  extension [Tup <: Tuple](tuple: Tup) inline def toArrayOf[T: ClassTag]: Array[T] = tuple match {
    case EmptyTuple => new Array[T](0)
    case self: Product =>
      val arr = new Array[T](self.productArity)
      var i = 0
      while (i < arr.length) {
        arr(i) = self.productElement(i).asInstanceOf[T]
        i += 1
      }
      arr
  }
}
