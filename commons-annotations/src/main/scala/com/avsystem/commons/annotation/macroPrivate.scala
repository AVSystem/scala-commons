package com.avsystem.commons
package annotation

import scala.annotation.StaticAnnotation

/**
  * Symbols annotated with this annotation can only be used in macro-generated code.
  */
class macroPrivate extends StaticAnnotation
