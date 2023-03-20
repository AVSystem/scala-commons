package com.avsystem.commons
package macros

trait CompatMacroCommons { this: MacroCommons =>
  type NamedArgTree = c.universe.NamedArg
  final val NamedArgTree = c.universe.NamedArg
}
