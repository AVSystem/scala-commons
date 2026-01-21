package com.avsystem.commons
package derivation

trait DeferredInstance[T] { this: T =>
  var underlying: T = null.asInstanceOf[T]
}
