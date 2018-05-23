package com.avsystem.commons
package rpc

import scala.annotation.StaticAnnotation

/**
  * Annotations that extend this trait will be retained for runtime in `RPCMetadata` typeclass instances
  */
trait MetadataAnnotation extends StaticAnnotation

trait MethodMetadata[T]
trait ParamMetadata[T]
