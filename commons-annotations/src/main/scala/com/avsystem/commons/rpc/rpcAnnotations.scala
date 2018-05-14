package com.avsystem.commons
package rpc

import com.avsystem.commons.annotation.AnnotationAggregate

import scala.annotation.StaticAnnotation

/**
  * For annotations applied on real RPC traits.
  */
trait RPCAnnotation extends StaticAnnotation

/**
  * For annotations applied on raw RPC traits that specify how real methods are matched against raw methods.
  */
sealed trait RPCMetaAnnotation extends StaticAnnotation

/**
  * You can use this annotation on overloaded RPC methods to give them unique identifiers for RPC serialization.
  * You can also subclass this annotation provided that you always override the `name` parameter with another
  * constructor parameter.
  */
class RPCName(val name: String) extends RPCAnnotation

trait VerbatimByDefault extends AnnotationAggregate {
  @verbatim
  type Implied
}

sealed trait RpcArity extends RPCMetaAnnotation
final class single extends RpcArity with VerbatimByDefault
final class optional extends RpcArity with VerbatimByDefault
final class repeated extends RpcArity
final class namedRepeated extends RpcArity

sealed trait RpcEncoding extends RPCMetaAnnotation
final class encoded extends RpcEncoding
final class verbatim extends RpcEncoding

sealed trait RpcFilter extends RPCMetaAnnotation
final class annotatedWith[A <: RPCAnnotation] extends RpcFilter
