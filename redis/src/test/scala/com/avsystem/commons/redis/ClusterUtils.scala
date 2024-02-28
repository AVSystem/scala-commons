package com.avsystem.commons
package redis

import org.apache.pekko.util.ByteString

import scala.io.Source

/**
  * Author: ghik
  * Created: 28/06/16.
  */
object ClusterUtils {
  final val SlotKeys =
    Source.fromInputStream(getClass.getResourceAsStream("/slotkeys.txt"))
      .getLines().toArray

  def keyWithSameSlotAs(key: String): String =
    SlotKeys(Hash.slot(ByteString(key)))

  def strings(length: Int): Iterator[String] =
    if (length == 0) Iterator("")
    else strings(length - 1).flatMap(p => ('a' to 'z').iterator.map(c => p + c))

  def findKeyForSlot(slot: Int, length: Int): String =
    strings(length).find(bs => Hash.slot(ByteString(bs)) == slot)
      .getOrElse(throw new IllegalArgumentException(s"Could not find key that would map to slot $slot"))

  def main(args: Array[String]): Unit = {
    println(SlotKeys(123))
  }
}
