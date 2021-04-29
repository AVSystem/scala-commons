package com.avsystem.commons
package serialization

/**
  * Can be used on case classes with exactly one field to instruct automatically generated `GenCodec` that the
  * class is a "transparent wrapper" and should be serialized to the same representation as the value of its sole
  * field.
  *
  * Whenever possible, it's better to use [[TransparentWrapperCompanion]] rather than this annotation.
  * [[TransparentWrapperCompanion]] will give you more typeclass instances for free (e.g. `GenKeyCodec` in addition to
  * just `GenCodec`) while this annotation requires special macro support from every typeclass.
  */
class transparent extends StaticAnnotation
