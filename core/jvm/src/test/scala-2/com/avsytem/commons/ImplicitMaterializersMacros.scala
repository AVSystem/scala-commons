package com.avsystem.commons
package macros

trait ImplicitMaterializersMacros [TC[_]]{ this: TC.type =>
    implicit def materializeImplicitly[T](implicit allow: AllowImplicitMacro[TC[T]]): TC[T] =
      macro macros.TestMacros.materializeImplicitly[T]
  }