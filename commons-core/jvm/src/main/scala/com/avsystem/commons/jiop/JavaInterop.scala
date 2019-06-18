package com.avsystem.commons
package jiop

trait JavaInterop extends AnyRef
  with JBasicUtils
  with JCollectionUtils
  with CompatAsJavaScalaExtensions
  with Java8CollectionUtils
  with JFunctionUtils
  with JStreamUtils
  with JOptionalUtils
  with JavaTimeInterop

object JavaInterop extends JavaInterop
