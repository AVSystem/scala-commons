package com.avsystem.commons
package misc

object CrossUtils {
  type NativeArray[A] = Array[A]
  type NativeDict[A] = MMap[String, A]

  def newNativeArray[A: ClassTag](size: Int): NativeArray[A] = new Array[A](size)
  def newNativeDict[A]: NativeDict[A] = new MHashMap[String, A]
  def unsetArrayValue: Any = null

  def wrappedArray[A: ClassTag](elems: A*): MIndexedSeq[A] = Array(elems*)
  def arrayBuffer[A]: MIndexedSeq[A] & MBuffer[A] = new MArrayBuffer[A]
  def dictionary[A](keyValues: (String, A)*): MMap[String, A] = MHashMap[String, A](keyValues*)
}
