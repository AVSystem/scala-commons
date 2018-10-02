package com.avsystem.commons
package jetty.rest.examples

import com.avsystem.commons.rest._
import com.avsystem.commons.serialization.GenCodec

trait GenericApi[T] {
  def process(value: T): Future[T]
}
object GenericApi {
  import DefaultRestImplicits._
  implicit def restAsRawReal[T: GenCodec]: RawRest.AsRawRealRpc[GenericApi[T]] = RawRest.materializeAsRawReal
  implicit def restMetadata[T]: RestMetadata[GenericApi[T]] = RestMetadata.materialize

  import openapi._
  implicit def openApiMetadata[T: RestSchema]: OpenApiMetadata[GenericApi[T]] = OpenApiMetadata.materialize
}