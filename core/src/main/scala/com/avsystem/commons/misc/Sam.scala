package com.avsystem.commons
package misc

@deprecated(
  "Use native SAM conversion instead, e.g. `val r: Runnable = () => doStuff()` or `val c: JConsumer[T] = t => ...`",
  "2.28.0",
)
object Sam {
  // TODO[scala3-port]: Sam.apply (Scala 2 macro def) (L)
  def apply[T](fun: => Any): T = ???
}
