package com.avsystem.commons
package misc

class MacroGenerated[T](val forCompanion: Any => T)
object MacroGenerated {
  def apply[T](instance: => T): MacroGenerated[T] =
    new MacroGenerated[T](_ => instance)
}
