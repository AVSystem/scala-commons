package com.avsystem.commons
package testutil

import com.avsystem.commons.macros.TestMacros
import org.scalatest.Assertions

trait CompilationErrorAssertions extends Assertions {
  def typeErrorFor(code: String): String = ???
}
