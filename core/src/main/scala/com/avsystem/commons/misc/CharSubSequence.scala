package com.avsystem.commons
package misc

final class CharSubSequence private(
  private val seq: CharSequence,
  private val start: Int,
  private val end: Int
) extends CharSequence {
  def length: Int = end - start

  def charAt(index: Int): Char =
    if (index >= 0 && index < length) seq.charAt(start + index)
    else throw new IndexOutOfBoundsException(index.toString)

  def subSequence(subStart: Int, subEnd: Int): CharSequence =
    if (subStart < 0 || subEnd > length || subStart > subEnd) throw new IllegalArgumentException
    else new CharSubSequence(seq, start + subStart, start + subEnd)

  override def toString: String = {
    val res = new JStringBuilder
    res.append(seq, start, end)
    res.toString
  }
}
object CharSubSequence {
  /**
    * Creates a subsequence of a `CharSequence` which is guaranteed to be a wrapper and never a copy
    * (unlike standard `subSequence` method).
    */
  def apply(seq: CharSequence, start: Int, end: Int): CharSequence =
    if (start < 0 || end > seq.length || start > end) throw new IllegalArgumentException
    else if (start == 0 && end == seq.length) seq
    else new CharSubSequence(seq, start, end)
}

