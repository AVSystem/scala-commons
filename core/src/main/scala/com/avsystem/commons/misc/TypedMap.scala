package com.avsystem.commons.misc

import com.avsystem.commons.SharedExtensions._
import com.avsystem.commons.misc.TypedMap.GenCodecMapping
import com.avsystem.commons.serialization._

/** A map whose keys are parameterized with value type. This makes it possible to associate different value type with
  * each key, in a type-safe way.
  *
  * [[TypedMap[K]]] has a [[GenCodec]] instance as long as there is a `GenCodec[K[_]]` instance for the key type and a
  * [[GenCodecMapping[K]]] instance that determines the codec for the value type associated with given key.
  *
  * Example:
  * {{{
  *   sealed abstract class AttributeKey[T](implicit val valueCodec: GenCodec[T])
  *     extends TypedKey[T] with AutoNamedEnum
  *
  *   object AttributeKey extends NamedEnumCompanion[AttributeKey[_]] {
  *     object StringKey extends AttributeKey[String]
  *     object IntKey extends AttributeKey[Int]
  *
  *     val values: List[AttributeKey[_]] = caseObjects
  *   }
  *
  *   val attributes = TypedMap[AttributeKey](
  *     AttributeKey.StringKey -> "foo",
  *     AttributeKey.IntKey -> 42,
  *   )
  * }}}
  *
  * Note that since all keys and value types are known statically, the map above is somewhat equivalent to a case class:
  *
  * {{{
  *   case class Attributes(
  *     string: Opt[String],
  *     int: Opt[Int]
  *   )
  * }}}
  *
  * [[TypedMap]] might be a good choice if there is a lot of attribute keys, they aren't statically known or some
  * collection-like behaviour is necessary (e.g. computing the size, iterating over all elements). A [[TypedMap]] is
  * also easier to evolve than a case class (e.g. because of binary compatibility issues).
  */
class TypedMap[K[_]](val raw: Map[K[_], Any]) extends AnyVal {
  def apply[T](key: K[T]): T =
    raw(key).asInstanceOf[T]

  def get[T](key: K[T]): Option[T] =
    raw.get(key).asInstanceOf[Option[T]]

  def getOpt[T](key: K[T]): Opt[T] =
    raw.getOpt(key).asInstanceOf[Opt[T]]

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

  override def toString: String = s"TypedMap($raw)"
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

  implicit def typedMapCodec[K[_]](implicit keyCodec: GenKeyCodec[K[_]], codecMapping: GenCodecMapping[K])
    : GenObjectCodec[TypedMap[K]] =
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

/** Base class for key types of [[TypedMap]] (typically enums parameterized by value type). Provides an instance of
  * [[GenCodecMapping]] which is necessary for [[GenCodec]] instance for [[TypedMap]] that uses this key type.
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
