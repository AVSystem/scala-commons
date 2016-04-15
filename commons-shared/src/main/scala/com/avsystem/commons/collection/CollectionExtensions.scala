package com.avsystem.commons
package collection

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

/**
  * Author: ghik
  * Created: 08/04/16.
  */
object CollectionExtensions {
  implicit class iteratorOps[A](private val it: Iterator[A]) extends AnyVal {
    def drainTo[C[_]](n: Int)(implicit cbf: CanBuildFrom[Nothing, A, C[A]]): C[A] = {
      val builder = cbf()
      var i = 0
      while (it.hasNext && i < n) {
        builder += it.next()
        i += 1
      }
      builder.result()
    }
  }
}
