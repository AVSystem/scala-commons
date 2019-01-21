package com.avsystem.commons
package serialization

/**
  * Can be used on case classes with exactly one field to instruct automatically generated `GenCodec` that the
  * class is a "transparent wrapper" and should be serialized to the same representation as the value of its sole
  * field.
  */
class transparent extends StaticAnnotation
