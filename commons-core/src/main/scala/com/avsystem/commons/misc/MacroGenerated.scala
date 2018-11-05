package com.avsystem.commons
package misc

/**
  * Wrapper class for macro-generated typeclasses. Usually, a typeclass is wrapped in `MacroGenerated` when
  * it's accepted as implicit super constructor parameter of some base class for companion objects of
  * types for which the typeclass is being generated.
  * Example: [[com.avsystem.commons.serialization.HasGenCodec HasGenCodec]], which is a base class for
  * companion objects of classes that want [[com.avsystem.commons.serialization.GenCodec GenCodec]] to be
  * macro-generated for them.
  *
  * Instead of materializing the type class instance directly, a function from some base companion type `C` is
  * materialized. To obtain the actual typeclass instance, companion object must be passed as this function's
  * argument. This serves two purposes:
  *
  * - contents of `C` will be wildcard-imported into macro-materialization, allowing injection of additional implicits
  * - working around too strict Scala validation of super constructor arguments: https://github.com/scala/bug/issues/7666
  */
@deprecated("Use MacroInstances instead", "1.34")
class MacroGenerated[C, T](val forCompanion: C => T) extends AnyVal
object MacroGenerated {
  def apply[C, T](instance: => T): MacroGenerated[C, T] =
    new MacroGenerated[C, T](_ => instance)
}
