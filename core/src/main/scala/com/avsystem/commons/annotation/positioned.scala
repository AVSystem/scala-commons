package com.avsystem.commons
package annotation

/** Annotate a symbol (i.e. class, method, parameter, etc.) with `@positioned(positioned.here)` to retain source
  * position information for that symbol to be available in macro implementations which inspect that symbol. This is
  * necessary e.g. for determining declaration order of subtypes of sealed hierarchies in macro implementations. This
  * annotation is only needed when macro is invoked in a different source file than the source file of inspected symbol.
  * If macro is invoked in the same file, source position is always available.
  */
class positioned(val point: Int) extends StaticAnnotation
object positioned {
  def here: Int = macro macros.misc.MiscMacros.posPoint
}
