package com.avsystem.commons
package meta

import com.avsystem.commons.meta.Mapping.ConcatIterable
import com.avsystem.commons.serialization.GenCodec

import scala.collection.generic.CanBuildFrom
import scala.collection.{MapLike, mutable}

/**
  * Simple immutable structure to collect named values while retaining their order and
  * providing fast, hashed lookup by name when necessary.
  * Intended to be used for [[multi]] raw parameters.
  */
final class Mapping[+V](private val wrapped: IIterable[(String, V)])
  extends IMap[String, V] with MapLike[String, V, Mapping[V]] {

  private[this] lazy val vector = {
    val keys = new mutable.HashSet[String]
    wrapped.iterator.filter({ case (k, _) => keys.add(k) }).toVector
  }
  private[this] lazy val map =
    wrapped.toMap

  override def empty: Mapping[V] = Mapping.empty
  override protected[this] def newBuilder: mutable.Builder[(String, V), Mapping[V]] =
    Mapping.newBuilder[V]

  override def size: Int =
    vector.size
  override def iterator: Iterator[(String, V)] =
    vector.iterator
  override def valuesIterator: Iterator[V] =
    vector.iterator.map({ case (_, v) => v })
  override def keys: Iterable[String] =
    map.keys
  override def keySet: ISet[String] =
    map.keySet
  override def contains(key: String): Boolean =
    map.contains(key)
  override def isDefinedAt(key: String): Boolean =
    map.isDefinedAt(key)
  override def applyOrElse[A1 <: String, B1 >: V](key: A1, default: A1 => B1): B1 =
    map.applyOrElse(key, default)
  override def apply(key: String): V =
    map.apply(key)

  def get(key: String): Option[V] = map.get(key)

  def -(key: String): Mapping[V] = Mapping(vector.filter({ case (k, _) => k == key }))

  def +[V0 >: V](pair: (String, V0)): Mapping[V0] =
    Mapping(wrapped ++ List(pair))

  def ++[V0 >: V](other: Mapping[V0]): Mapping[V0] =
    if (wrapped.isEmpty) other
    else if (other.wrapped.isEmpty) this
    else new Mapping(ConcatIterable(wrapped, other.wrapped))
}
object Mapping {
  def empty[V]: Mapping[V] = new Mapping(Nil)
  def apply[V](pairs: (String, V)*): Mapping[V] = new Mapping(pairs.toList)
  def apply[V](pairs: IIterable[(String, V)]): Mapping[V] = new Mapping(pairs)

  def newBuilder[V]: mutable.Builder[(String, V), Mapping[V]] =
    new MListBuffer[(String, V)].mapResult(new Mapping(_))

  private case class ConcatIterable[+V](first: IIterable[V], second: IIterable[V]) extends IIterable[V] {
    def iterator: Iterator[V] = first.iterator ++ second.iterator
  }

  private val reusableCBF = new CanBuildFrom[Nothing, (String, Any), Mapping[Any]] {
    def apply(from: Nothing): mutable.Builder[(String, Any), Mapping[Any]] = newBuilder[Any]
    def apply(): mutable.Builder[(String, Any), Mapping[Any]] = newBuilder[Any]
  }

  implicit def canBuildFrom[V]: CanBuildFrom[Nothing, (String, V), Mapping[V]] =
    reusableCBF.asInstanceOf[CanBuildFrom[Nothing, (String, V), Mapping[V]]]

  implicit def genCodec[V: GenCodec]: GenCodec[Mapping[V]] = GenCodec.createNullableObject(
    oi => new Mapping(oi.iterator(GenCodec.read[V]).toList),
    (oo, np) => np.foreach({ case (k, v) => GenCodec.write[V](oo.writeField(k), v) })
  )
}
