package com.avsystem.commons
package misc

import com.avsystem.commons.misc.TypedMap.GenCodecMapping
import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec, ObjectInput, ObjectOutput}

import scala.language.{higherKinds, implicitConversions}

class TypedMap[K[_]](val raw: Map[K[_], Any]) extends AnyVal {
  def apply[T](key: K[T]): Option[T] =
    raw.get(key).map(_.asInstanceOf[T])

  def updated[T](key: K[T], value: T): TypedMap[K] =
    new TypedMap[K](raw.updated(key, value))

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

  implicit def typedMapCodec[K[_]](implicit keyCodec: GenKeyCodec[K[_]], codecMapping: GenCodecMapping[K]): GenCodec[TypedMap[K]] =
    new GenCodec.ObjectCodec[TypedMap[K]] {
      def nullable = false
      def readObject(input: ObjectInput): TypedMap[K] = {
        val rawBuilder = Map.newBuilder[K[_], Any]
        while (input.hasNext) {
          val fieldInput = input.nextField()
          val key = keyCodec.read(fieldInput.fieldName)
          rawBuilder += ((key, codecMapping.valueCodec(key).read(fieldInput)))
        }
        new TypedMap[K](rawBuilder.result())
      }
      def writeObject(output: ObjectOutput, typedMap: TypedMap[K]): Unit =
        typedMap.raw.foreach { case (key, value) =>
          codecMapping.valueCodec(key.asInstanceOf[K[Any]]).write(output.writeField(keyCodec.write(key)), value)
        }
    }
}

/**
  * Base class for sealed enums which can be used as key type for a [[TypedMap]]. It also ensures that
  * the [[TypedMap]] using [[TypedKey]] as a key type will always have a `GenCodec`.
  */
abstract class TypedKey[T](implicit val valueCodec: GenCodec[T])
trait TypedKeyCompanion[K[X] <: TypedKey[X]] extends SealedEnumCompanion[K[_]] {
  /**
    * `GenKeyCodec` for typed key `K`.
    * You can implement this with `GenKeyCodec.forSealedEnum` macro
    */
  implicit def keyCodec: GenKeyCodec[K[_]]

  implicit val codecMapping: GenCodecMapping[K] =
    new GenCodecMapping[K] {
      def valueCodec[T](key: K[T]) = key.valueCodec
    }
}
