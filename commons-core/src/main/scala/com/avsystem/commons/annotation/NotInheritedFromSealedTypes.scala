package com.avsystem.commons
package annotation

/**
  * Marker trait for annotations which don't want to be inherited by subtypes
  * of a sealed trait or class that has this annotation applied. Intended for annotations that should apply
  * only to the sealed trait itself.
  */
trait NotInheritedFromSealedTypes extends StaticAnnotation
