package com.avsystem.commons
package jiop

import scala.collection.convert.{DecorateAsJava, DecorateAsScala}

trait JavaInterop extends AnyRef
  with JBasicUtils
  with JCollectionUtils
  with DecorateAsJava
  with DecorateAsScala

object JavaInterop extends JavaInterop
