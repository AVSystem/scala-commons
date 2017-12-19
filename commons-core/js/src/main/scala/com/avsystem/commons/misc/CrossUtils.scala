package com.avsystem.commons
package misc

import scala.scalajs.js

object CrossUtils {
  def wrappedArray[A: ClassTag](elems: A*): MIndexedSeq[A] = js.Array(elems: _*)
  def arrayBuffer[A]: MIndexedSeq[A] with MBuffer[A] = js.Array[A]()
  def dictionary[A](keyValues: (String, A)*): MMap[String, A] = js.Dictionary(keyValues: _*)
}
