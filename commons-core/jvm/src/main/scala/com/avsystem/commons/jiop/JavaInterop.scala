package com.avsystem.commons
package jiop

import scala.collection.convert.{DecorateAsJava, DecorateAsScala}

trait JavaInterop extends AnyRef
  with JBasicUtils
  with JCollectionUtils
  with DecorateAsJava
  with DecorateAsScala
  with Java8CollectionUtils
  with JFunctionUtils
  with JStreamUtils
  with JOptionalUtils
  with JavaTimeInterop

object JavaInterop extends JavaInterop
