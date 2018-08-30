package com.avsystem.commons
package misc

import org.scalatest.FunSuite

object Obj {
  val x: String = "fuu"

  class Inner {
    val x: String = "fuu"

    def valueOfX: x.type = ValueOf[x.type]
    def valueOfThis: this.type = ValueOf[this.type]
  }
}

class ValueOfTest extends FunSuite {
  test("object") {
    assert(ValueOf[Obj.type] == Obj)
  }

  test("static val") {
    assert(ValueOf[Obj.x.type] == Obj.x)
  }

  test("inner val of local") {
    val i = new Obj.Inner
    assert(ValueOf[i.x.type] == i.x)
    assert(i.valueOfX == i.x)
  }

  test("this") {
    val i = new Obj.Inner
    assert(i.valueOfThis == i)
  }
}
