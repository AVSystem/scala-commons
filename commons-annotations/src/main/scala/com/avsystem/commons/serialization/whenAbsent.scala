package com.avsystem.commons
package serialization

import scala.annotation.StaticAnnotation

/**
  * An alternative way to provide default value for case class parameter used during deserialization with `GenCodec`
  * when its field is missing in data being deserialized. Normally, Scala-level default parameter values are picked
  * up, but you may want to use this annotation instead if you don't want to pollute your Scala classes with
  * unintended default parameter values (i.e. you want a default value *only* for deserialization).
  *
  * {{{
  *   case class HasDefault(@whenAbsent("default") str: String)
  *   object HasDefault extends HasGenCodec[HasDefault]
  * }}}
  *
  * If a parameter has both Scala-level default value and is annotated with `@whenAbsent` then value from annotation
  * takes priority. You can use this to have different source-level default value and different
  * default value for deserialization. You can also leverage this to "remove" default value for deserialization:
  *
  * {{{
  *   case class HasNoDefault(@whenAbsent(throw new Exception) str: String = "default")
  *   object HasDefault extends HasGenCodec[HasDefault]
  * }}}
  */
class whenAbsent[+T](v: => T) extends StaticAnnotation {
  def value: T = v
}
