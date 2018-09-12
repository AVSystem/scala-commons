package com.avsystem.commons
package rest.openapi

import scala.annotation.StaticAnnotation

sealed trait Adjuster extends StaticAnnotation

/**
  * Base trait for annotations which may adjust [[Schema]] generated for a type from its [[RestStructure]].
  * Schema adjuster annotations only have effect on types for which [[RestStructure]] is macro-generated and
  * [[RestSchema]] is derived from [[RestStructure]] (e.g. types with companions extending [[RestDataCompanion]]).
  * Schema adjusters have no effect on methods or parameters. Also note that they have no effect on types annotated as
  * [[com.avsystem.commons.serialization.transparent transparent]] where schema is taken directly from the wrapped
  * type, without changes.
  */
trait SchemaAdjuster extends Adjuster {
  def adjustSchema(schema: Schema): Schema
}

/**
  * Base trait for annotation which may adjust [[Parameter]] generated for path, query or header parameters
  * of REST RPC methods.
  */
trait ParameterAdjuster extends Adjuster {
  def adjustParameter(parameter: Parameter): Parameter
}

/**
  * Base trait for annotation which may adjust [[Operation]] generated for path, query or header parameters
  * of REST RPC methods.
  */
trait OperationAdjuster extends Adjuster {
  def adjustOperation(operation: Operation): Operation
}

/** Convenience implementation of [[SchemaAdjuster]] */
class adjustSchema(f: Schema => Schema) extends SchemaAdjuster {
  def adjustSchema(value: Schema): Schema = f(value)
}
/** Convenience implementation of [[ParameterAdjuster]] */
class adjustParameter(f: Parameter => Parameter) extends ParameterAdjuster {
  def adjustParameter(value: Parameter): Parameter = f(value)
}
/** Convenience implementation of [[OperationAdjuster]] */
class adjustOperation(f: Operation => Operation) extends OperationAdjuster {
  def adjustOperation(value: Operation): Operation = f(value)
}

class description(desc: String) extends SchemaAdjuster with ParameterAdjuster with OperationAdjuster {
  def adjustSchema(schema: Schema): Schema = schema.copy(description = desc)
  def adjustParameter(parameter: Parameter): Parameter = parameter.copy(description = desc)
  def adjustOperation(operation: Operation): Operation = operation.copy(description = desc)
}
