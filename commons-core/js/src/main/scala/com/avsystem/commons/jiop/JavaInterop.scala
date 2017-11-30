package com.avsystem.commons
package jiop

import scala.collection.convert.{DecorateAsJava, DecorateAsScala}

trait JavaInterop extends Any
  with JBasicUtils
  with JCollectionUtils
  // contents of scala.collection.JavaConverters
  with DecorateAsJava
  with DecorateAsScala

object JavaInterop extends JavaInterop
