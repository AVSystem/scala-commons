package com.avsystem.commons
package jiop

import scala.collection.convert.{DecorateAsJava, DecorateAsScala}

object JavaInterop
  extends JBasicUtils
    with JFunctionUtils
    with JOptionalUtils
    with JStreamUtils
    with GuavaUtils
    with Java8CollectionUtils
    // contents of scala.collection.JavaConverters
    with DecorateAsJava
    with DecorateAsScala

