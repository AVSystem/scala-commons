package com.avsystem.commons
package jiop

import scala.collection.convert.{DecorateAsJava, DecorateAsScala}

object JavaInterop
  extends JBasicUtils
  with JFunctionUtils
  with JOptionalUtils
  with JStreamUtils
  with GuavaUtils
  with JCollectionUtils
  // contents of scala.collection.JavaConverters
  with DecorateAsJava
  with DecorateAsScala
