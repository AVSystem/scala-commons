package com.avsystem.commons
package jiop

trait JavaInterop
  extends BasicJavaInterop
    with Java8Interop
    with GuavaUtils

object JavaInterop extends JavaInterop

trait Java8Interop
  extends Java8CollectionUtils
    with JFunctionUtils
    with JStreamUtils
    with JOptionalUtils

object Java8Interop extends Java8Interop
object GuavaInterop extends GuavaUtils
