package com.avsystem.commons
package misc

import scala.scalajs.js

object CrossUtils {
  type NativeArray[A] = js.Array[A]
  type NativeDict[A] = js.Dictionary[A]

  def newNativeArray[A](size: Int): NativeArray[A] = {
    val res = new js.Array[A](size)
    var idx = 0
    while (idx < size) {
      res(idx) = null.asInstanceOf[A]
      idx += 1
    }
    res
  }

  def newNativeDict[A]: NativeDict[A] = js.Dictionary.empty[A]

  def wrappedArray[A](elems: A*): MIndexedSeq[A] = js.Array(elems*)
  def arrayBuffer[A]: MIndexedSeq[A] & MBuffer[A] = js.Array[A]()
  def dictionary[A](keyValues: (String, A)*): MMap[String, A] = js.Dictionary(keyValues*)
}
