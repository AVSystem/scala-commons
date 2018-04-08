package com.avsystem.commons
package rpc

import scala.annotation.{Annotation, StaticAnnotation}

/**
  * @author ghik
  */
class annotatedWith[A <: Annotation] extends StaticAnnotation
