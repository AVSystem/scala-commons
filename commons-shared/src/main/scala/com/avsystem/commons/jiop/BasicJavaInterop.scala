package com.avsystem.commons
package jiop

import scala.collection.convert.{DecorateAsJava, DecorateAsScala}

object BasicJavaInterop
  extends JBasicUtils
    with JCollectionUtils
    // contents of scala.collection.JavaConverters
    with DecorateAsJava
    with DecorateAsScala
