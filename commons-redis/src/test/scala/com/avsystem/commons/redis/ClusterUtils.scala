package com.avsystem.commons
package redis

import akka.util.ByteString

import scala.io.Source

/**
  * Author: ghik
  * Created: 28/06/16.
  */
object ClusterUtils {
  val SlotKeys =
    Source.fromInputStream(getClass.getResourceAsStream("/slotkeys.txt"))
      .getLines().map(ByteString(_)).toArray

  def keyWithSameSlotAs(key: ByteString) =
    SlotKeys(Hash.slot(key))

  def strings(length: Int): Iterator[String] =
    if (length == 0) Iterator("")
    else strings(length - 1).flatMap(p => ('a' to 'z').iterator.map(c => p + c))

  def findKeyForSlot(slot: Int, length: Int): ByteString =
    strings(length).map(ByteString(_)).find(bs => Hash.slot(bs) == slot)
      .getOrElse(throw new IllegalArgumentException(s"Could not find key that would map to slot $slot"))

  def main(args: Array[String]): Unit = {
    println(SlotKeys(123))
  }
}
