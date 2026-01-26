package com.avsystem.commons
package testutil

import org.scalatest.Assertions

trait CompilationErrorAssertions extends Assertions {
  def typeErrorFor(code: String): String = ???
}
