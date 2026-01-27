package com.avsystem.commons

trait CommonAliases {
  export scala.util.Try
  export scala.util.Success
  export scala.util.Failure

  export scala.concurrent.Future
  export scala.concurrent.Promise
  export scala.concurrent.ExecutionContext

  export scala.util.control.NonFatal

  export scala.reflect.ClassTag
  export scala.reflect.classTag

  export scala.annotation.Annotation
  export scala.annotation.StaticAnnotation

  export scala.quoted.{Expr, Quotes, Type}
  export scala.deriving.Mirror
}
object CommonAliases extends CommonAliases
