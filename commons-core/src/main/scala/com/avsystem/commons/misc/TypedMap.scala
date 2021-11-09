package com.avsystem.commons
package misc

import com.avsystem.commons.misc.TypedMap.GenCodecMapping
import com.avsystem.commons.serialization._

class TypedMap[K[_]](val raw: Map[K[_], Any]) extends AnyVal {
  def apply[T](key: K[T]): T =
    raw(key).asInstanceOf[T]

  def get[T](key: K[T]): Option[T] =
    raw.get(key).asInstanceOf[Option[T]]

  def getOrElse[T](key: K[T], defaultValue: => T): T =
    get(key).getOrElse(defaultValue)

  def updated[T](key: K[T], value: T): TypedMap[K] =
    new TypedMap[K](raw.updated(key, value))

  def ++(other: TypedMap[K]): TypedMap[K] =
    new TypedMap[K](raw ++ other.raw)

  def keys: Iterable[K[_]] = raw.keys
  def keysIterator: Iterator[K[_]] = raw.keysIterator
  def keySet: Set[K[_]] = raw.keySet

  def size: Int = raw.size

  override def toString = s"TypedMap($raw)"
}

object TypedMap {
  case class Entry[K[_], T](pair: (K[T], T))
  object Entry {
    implicit def pairToEntry[K[_], T](pair: (K[T], T)): Entry[K, T] = Entry(pair)
  }

  def empty[K[_]]: TypedMap[K] =
    new TypedMap[K](Map.empty)

  def apply[K[_]](entries: Entry[K, _]*): TypedMap[K] = {
    val raw = Map.newBuilder[K[_], Any]
    entries.foreach(e => raw += e.pair)
    new TypedMap[K](raw.result())
  }

  trait GenCodecMapping[K[_]] {
    def valueCodec[T](key: K[T]): GenCodec[T]
  }

  implicit def typedMapCodec[K[_]](implicit
    keyCodec: GenKeyCodec[K[_]],
    codecMapping: GenCodecMapping[K]
  ): GenObjectCodec[TypedMap[K]] =
    new GenCodec.ObjectCodec[TypedMap[K]] {
      def nullable = false
      def readObject(input: ObjectInput): TypedMap[K] = {
        val rawBuilder = Map.newBuilder[K[_], Any]
        input.knownSize match {
          case -1 =>
          case size => rawBuilder.sizeHint(size)
        }
        while (input.hasNext) {
          val fieldInput = input.nextField()
          val key = keyCodec.read(fieldInput.fieldName)
          rawBuilder += ((key, codecMapping.valueCodec(key).read(fieldInput)))
        }
        new TypedMap[K](rawBuilder.result())
      }
      def writeObject(output: ObjectOutput, typedMap: TypedMap[K]): Unit = {
        output.declareSizeOf(typedMap.raw)
        typedMap.raw.foreach { case (key, value) =>
          val valueCodec = codecMapping.valueCodec(key.asInstanceOf[K[Any]])
          valueCodec.write(output.writeField(keyCodec.write(key)), value)
        }
      }
    }
}

/**
  * Base class for key types of [[TypedMap]] (typically enums parameterized by value type).
  * Provides an instance of [[GenCodecMapping]] which is necessary for [[GenCodec]] instance for [[TypedMap]] that
  * uses this key type.
  */
trait TypedKey[T] {
  def valueCodec: GenCodec[T]
}
object TypedKey {
  implicit def codecMapping[K[X] <: TypedKey[X]]: GenCodecMapping[K] =
    new GenCodecMapping[K] {
      def valueCodec[T](key: K[T]): GenCodec[T] = key.valueCodec
    }
}
