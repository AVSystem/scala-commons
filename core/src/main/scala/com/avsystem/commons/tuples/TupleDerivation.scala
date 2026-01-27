package com.avsystem.commons
package tuples

/**
 * IntelliJ-friendly (no whitebox macros) boilerplate strapping layer for deriving tuple type classes (`C`) based on
 * instances of that typeclass for tuple element types.
 */
trait TupleDerivation[C[_]] {
  case class ElementInstances[T, I](instances: I)
  object ElementInstances {
    inline given [Tup <: Tuple] => ElementInstances[Tup, Tuple.Map[Tup, C]] =
      new ElementInstances(compiletime.summonAll[Tuple.Map[Tup, C]])
  }
}
