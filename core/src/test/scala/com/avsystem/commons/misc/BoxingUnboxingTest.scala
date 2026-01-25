package com.avsystem.commons.misc

import com.avsystem.commons.JInteger

class BoxingUnboxingTest {
  val jint: JInteger|Null = Opt(42).boxedOrNull
}
