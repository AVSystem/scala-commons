package com.avsystem.commons
package serialization

/**
  * When materializing a `GenCodec` for sealed hierarchy with `@flatten` annotation, you can use this
  * annotation on one of case classes or objects to mark it as the default one. If during deserialization the
  * codec is unable to find the `_case` field and determine the case class/object to deserialize, it will try to
  * deserialize the data to the class/object marked with this annotation.
  *
  * This is useful for retaining backwards compatibility with serialized format when refactoring code and replacing
  * a simple case class with a sealed hierarchy.
  *
  * @param transient if `true`, the codec will also not emit the `_case` field during writing of default
  *                  case class/object (analogous to [[transientDefault]]).
  */
class defaultCase(val transient: Boolean) extends StaticAnnotation {
  def this() = this(transient = false)
}
