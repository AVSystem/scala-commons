package com.avsystem.commons

trait CommonAliases {
  type Try[+T] = scala.util.Try[T]
  final val Try = scala.util.Try
  type Success[+T] = scala.util.Success[T]
  final val Success = scala.util.Success
  type Failure[+T] = scala.util.Failure[T]
  final val Failure = scala.util.Failure

  type Future[+T] = scala.concurrent.Future[T]
  final val Future = scala.concurrent.Future
  type Promise[T] = scala.concurrent.Promise[T]
  final val Promise = scala.concurrent.Promise
  type ExecutionContext = scala.concurrent.ExecutionContext
  final val ExecutionContext = scala.concurrent.ExecutionContext

  final val NonFatal = scala.util.control.NonFatal

  type ClassTag[T] = scala.reflect.ClassTag[T]
  final val ClassTag = scala.reflect.ClassTag
  final def classTag[T: ClassTag]: ClassTag[T] = scala.reflect.classTag[T]

  type Annotation = scala.annotation.Annotation
  type StaticAnnotation = scala.annotation.StaticAnnotation

  type Opt[+T] = misc.Opt[T]
  final val Opt = misc.Opt
  type OptRef[+T >: Null] = misc.OptRef[T]
  final val OptRef = misc.OptRef
  type NOpt[+T] = misc.NOpt[T]
  final val NOpt = misc.NOpt
  type OptArg[+T] = misc.OptArg[T]
  final val OptArg = misc.OptArg
}

object CommonAliases extends CommonAliases
