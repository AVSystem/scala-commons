package com.avsystem.commons
package rest.openapi

import com.avsystem.commons.annotation.NotInheritedFromSealedTypes

import scala.annotation.StaticAnnotation

sealed trait Adjuster extends StaticAnnotation

/**
  * Base trait for annotations which may adjust [[Schema]] derived for various symbols in REST API traits.
  * Schema adjusters may be applied on:
  *
  * - Types for which [[RestStructure]] is macro materialized and [[RestSchema]] derived from it.
  * This includes all types with companion extending [[RestDataCompanion]].
  *
  * - Fields of case classes for which [[RestStructure]] is macro materialized.
  *
  * - Body parameters of REST methods (parameters tagged with [[com.avsystem.commons.rest.JsonBodyParam JsonBodyParam]]
  * or [[com.avsystem.commons.rest.Body Body]] annotations)
  *
  * Schema adjusters DO NOT WORK on REST methods themselves and their path/header/query parameters.
  * Instead, use [[OperationAdjuster]] and [[ParameterAdjuster]].
  *
  * Also, be aware that schema adjusters may also be applied on schema references. In such cases, the schema reference
  * is wrapped into a [[Schema]] object with `allOf` property containing the original reference. This effectively
  * allows you to extend the referenced schema but you cannot inspect it in the process.
  */
trait SchemaAdjuster extends Adjuster with NotInheritedFromSealedTypes {
  def adjustSchema(schema: Schema): Schema
}
object SchemaAdjuster {
  def adjustRef(adjusters: List[SchemaAdjuster], schema: RefOr[Schema]): RefOr[Schema] =
    if (adjusters.nonEmpty)
      adjusters.foldRight(schema.rewrapRefToAllOf)(_ adjustSchema _).unwrapSingleRefAllOf
    else schema
}

/**
  * Base trait for annotation which may adjust [[Parameter]] generated for path, query or header parameters
  * of REST RPC methods.
  */
trait ParameterAdjuster extends Adjuster {
  def adjustParameter(parameter: Parameter): Parameter
}

/**
  * Base trait for annotation which may adjust [[Operation]] generated for REST RPC methods which translate to
  * HTTP operations (i.e. it doesn't work for prefix methods).
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

/**
  * Annotation that specifies description that will be included into generated OpenAPI specification.
  * It can be applied on REST methods ([[OperationAdjuster]]), path/header/query parameters ([[ParameterAdjuster]]),
  * body parameters ([[SchemaAdjuster]]), case class fields ([[SchemaAdjuster]]) and ADTs for which [[RestStructure]]
  * is macro generated ([[SchemaAdjuster]]).
  */
class description(desc: String) extends SchemaAdjuster with ParameterAdjuster with OperationAdjuster {
  def adjustSchema(schema: Schema): Schema = schema.copy(description = desc)
  def adjustParameter(parameter: Parameter): Parameter = parameter.copy(description = desc)
  def adjustOperation(operation: Operation): Operation = operation.copy(description = desc)
}
