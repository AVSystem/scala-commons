package com.avsystem.commons
package derivation

trait DeferredInstance[T] { this: T =>
  var underlying: T = scala.compiletime.uninitialized
}
