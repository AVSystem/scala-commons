package com.avsystem.commons
package serialization

// this should be written in Java but it was rewritten to Scala in order to make Scala.js linker happy
object BuildablePojo {
  def builder(): Builder = new Builder

  final class Builder private[BuildablePojo] {
    private var str: String = scala.compiletime.uninitialized
    private var num: Int = 0
    private var flags: JList[Boolean] = new JArrayList
    private var cool: Boolean = true

    def setStr(str: String): Builder = {
      this.str = str
      this
    }

    def setNum(num: Int): Builder = {
      this.num = num
      this
    }

    def setFlags(flags: JList[Boolean]): Builder = {
      this.flags = flags
      this
    }

    def setCool(cool: Boolean): Builder = {
      this.cool = cool
      this
    }

    def build(): BuildablePojo =
      new BuildablePojo(str, num, flags, cool)
  }
}
final class BuildablePojo private (
  private val str: String,
  private val num: Int,
  private val flags: JList[Boolean],
  private val cool: Boolean,
) {
  def getStr(): String = str
  def getNum(): Int = num
  def getFlags(): JList[Boolean] = flags
  def isCool(): Boolean = cool

  override def equals(o: Any): Boolean = o match {
    case bp: BuildablePojo =>
      bp.str == str && bp.num == num && bp.flags == flags && bp.cool == cool
    case _ => false
  }

  override def hashCode: Int = (str, num, flags, cool).hashCode

  override def toString: String =
    s"BuildablePojo($str, $num, $flags, $cool)"
}
