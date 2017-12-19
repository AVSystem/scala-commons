package com.avsystem.commons
package misc

object CrossUtils {
  def wrappedArray[A: ClassTag](elems: A*): MIndexedSeq[A] = Array(elems: _*)
  def arrayBuffer[A]: MIndexedSeq[A] with MBuffer[A] = new MArrayBuffer[A]
  def dictionary[A](keyValues: (String, A)*): MMap[String, A] = MHashMap[String, A](keyValues: _*)
}
