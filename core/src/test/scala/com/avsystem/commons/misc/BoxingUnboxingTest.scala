package com.avsystem.commons
package misc

import com.avsystem.commons.JInteger

class BoxingUnboxingTest {
  val jint: JInteger | Null = Opt(42).boxedOrNull
}
