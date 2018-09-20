package com.avsystem.commons
package rest.openapi.adjusters

import com.avsystem.commons.annotation.NotInheritedFromSealedTypes
import com.avsystem.commons.meta.infer
import com.avsystem.commons.rest.JsonValue
import com.avsystem.commons.rest.openapi.{Operation, Parameter, RefOr, RestSchema, RestStructure, Schema}
import com.avsystem.commons.rpc.AsRaw

import scala.annotation.StaticAnnotation

sealed trait Adjuster extends StaticAnnotation

/**
  * Base trait for annotations which may adjust [[Schema]] derived for various symbols in REST API traits.
  * Schema adjusters may be applied on:
  *
  * - Types for which [[RestStructure]] is macro materialized and [[RestSchema]] derived from it.
  * This includes all types with companion extending
  * [[com.avsystem.commons.rest.RestDataCompanion RestDataCompanion]].
  *
  * - Fields of case classes for which [[RestStructure]] is macro materialized.
  *
  * - Body parameters of REST methods (parameters tagged with [[com.avsystem.commons.rest.BodyField BodyField]]
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
  * Operation adjusters may also be specified on prefix methods - this way they will be applied to all operations
  * generated for the result of this prefix method.
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

/**
  * Adds example to [[Parameter]] object generated for REST method parameter annotated with this annotation.
  */
class example[+T](value: T, @infer asJson: AsRaw[JsonValue, T] = infer.value) extends ParameterAdjuster {
  def adjustParameter(parameter: Parameter): Parameter =
    parameter.copy(example = asJson.asRaw(value))
}

/**
  * Allows setting custom `operationId` for [[Operation]] objects generated for REST HTTP methods.
  *
  * By default, `operationId` is set to method's [[com.avsystem.commons.rpc.rpcName rpcName]] which in turn
  * defaults to method's regular name. If method is overloaded, method name may be prepended with lowercased
  * HTTP method followed by underscore (e.g. "post_")
  */
class operationId(operationId: OptArg[String] = OptArg.Empty) extends OperationAdjuster {
  def adjustOperation(operation: Operation): Operation =
    operation.copy(operationId = operationId)
}

/**
  * Prefix methods may be annotated with this annotation to specify prefix that will be prepended to
  * `operationId` of all [[Operation]] objects generated for result of that prefix method.
  * By default, this prefix is prefix method's name with underscore,
  * so this annotation may be used in particular to set empty prefix.
  */
class operationIdPrefix(val prefix: String) extends StaticAnnotation
