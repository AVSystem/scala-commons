package com.avsystem.commons
package misc

import scala.collection.generic.CanBuildFrom
import scala.util.{Failure, Try}

object TryOps {

  import scala.language.higherKinds

  /** Simple version of `TryOps.traverse`. Transforms a `TraversableOnce[Try[A]]` into a `Try[TraversableOnce[A]]`.
    * Useful for reducing many `Try`s into a single `Try`.
    */
  def sequence[A, M[X] <: TraversableOnce[X]](in: M[Try[A]])(implicit cbf: CanBuildFrom[M[Try[A]], A, M[A]]): Try[M[A]] = {
    in.foldLeft(Try(cbf(in))) {
      case (f@Failure(e), Failure(newEx)) => e.addSuppressed(newEx); f
      case (tr, tb) => for (r <- tr; a <- tb) yield r += a
    }.map(_.result())
  }

  /** Transforms a `TraversableOnce[A]` into a `Try[TraversableOnce[B]]` using the provided function `A => Try[B]`.
    * For example, to apply a function to all items of a list:
    *
    * {{{
    *    val myTryList = TryOps.traverse(myList)(x => Try(myFunc(x)))
    * }}}
    */
  def traverse[A, B, M[X] <: TraversableOnce[X]](in: M[A])(fn: A => Try[B])(implicit cbf: CanBuildFrom[M[A], B, M[B]]): Try[M[B]] =
  in.map(fn).foldLeft(Try(cbf(in))) {
    case (f@Failure(e), Failure(newEx)) => e.addSuppressed(newEx); f
    case (tr, tb) => for (r <- tr; b <- tb) yield r += b
  }.map(_.result())

}
