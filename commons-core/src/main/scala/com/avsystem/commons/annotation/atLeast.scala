package com.avsystem.commons
package annotation

import scala.annotation.StaticAnnotation

/**
 * When applied on varargs parameter, indicates that at least some number of parameters is required.
 * This is later checked by the static analyzer.
 */
class atLeast(n: Int) extends StaticAnnotation
