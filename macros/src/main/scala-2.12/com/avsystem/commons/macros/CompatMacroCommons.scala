package com.avsystem.commons
package macros

trait CompatMacroCommons { this: MacroCommons =>
  type NamedArgTree = c.universe.AssignOrNamedArg
  final val NamedArgTree = c.universe.AssignOrNamedArg
}
