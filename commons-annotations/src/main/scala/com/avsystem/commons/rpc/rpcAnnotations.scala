package com.avsystem.commons
package rpc

import com.avsystem.commons.annotation.AnnotationAggregate

import scala.annotation.StaticAnnotation

/**
  * For annotations applied on real RPC traits.
  */
trait RpcAnnotation extends StaticAnnotation

/**
  * For annotations applied on raw RPC traits that specify how real methods are matched against raw methods.
  */
sealed trait RawRpcAnnotation extends StaticAnnotation
sealed trait RawMethodAnnotation extends RawRpcAnnotation
sealed trait RawParamAnnotation extends RawRpcAnnotation

/**
  * You can use this annotation on overloaded RPC methods to give them unique identifiers for RPC serialization.
  * You can also subclass this annotation provided that you always override the `name` parameter with another
  * constructor parameter.
  */
class rpcName(val name: String) extends RpcAnnotation

trait RpcTag extends RpcAnnotation

trait VerbatimByDefault extends AnnotationAggregate {
  @verbatim
  type Implied
}

sealed trait RpcArity extends RawParamAnnotation
final class single extends RpcArity with VerbatimByDefault
final class optional extends RpcArity with VerbatimByDefault
final class repeated extends RpcArity

sealed trait RpcEncoding extends RawMethodAnnotation with RawParamAnnotation
final class encoded extends RpcEncoding
final class verbatim extends RpcEncoding

final class methodTag[BaseTag <: RpcTag, DefaultTag <: BaseTag] extends RawRpcAnnotation
final class paramTag[BaseTag <: RpcTag, DefaultTag <: BaseTag] extends RawMethodAnnotation
final class tagged[Tag <: RpcTag] extends RawMethodAnnotation with RawParamAnnotation

final class auxiliary extends RawParamAnnotation
