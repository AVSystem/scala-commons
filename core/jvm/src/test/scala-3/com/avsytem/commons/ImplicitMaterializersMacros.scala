package com.avsystem.commons
package macros

import com.avsystem.commons.derivation.AllowImplicitMacro

trait ImplicitMaterializersMacros [TC[_]]{
  implicit def materializeImplicitly[T](implicit allow: AllowImplicitMacro[TC[T]]): TC[T] = ???
}
