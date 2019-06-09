package com.avsystem.commons
package jiop

import scala.collection.convert.{AsJavaExtensions, AsScalaExtensions}

trait JavaInterop extends AnyRef
  with JBasicUtils
  with JCollectionUtils
  with AsJavaExtensions
  with AsScalaExtensions
  with Java8CollectionUtils
  with JFunctionUtils
  with JStreamUtils
  with JOptionalUtils
  with JavaTimeInterop

object JavaInterop extends JavaInterop
