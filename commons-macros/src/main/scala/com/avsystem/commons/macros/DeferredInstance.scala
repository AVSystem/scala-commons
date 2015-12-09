package com.avsystem.commons
package macros

/**
  * Author: ghik
  * Created: 07/12/15.
  */
trait DeferredInstance[T] {
  this: T =>
  
  var underlying: T = _
}
