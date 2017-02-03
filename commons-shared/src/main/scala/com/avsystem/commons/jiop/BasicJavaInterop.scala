package com.avsystem.commons
package jiop

import scala.collection.convert.{DecorateAsJava, DecorateAsScala}

trait BasicJavaInterop
  extends JBasicUtils
    with JCollectionUtils
    // contents of scala.collection.JavaConverters
    with DecorateAsJava
    with DecorateAsScala

object BasicJavaInterop extends BasicJavaInterop
