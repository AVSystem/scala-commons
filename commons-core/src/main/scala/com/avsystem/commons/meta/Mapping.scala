package com.avsystem.commons
package meta

import com.avsystem.commons.meta.Mapping.{ConcatIterable, KeyFilteredIterable}
import com.avsystem.commons.serialization.GenCodec

import scala.collection.generic.CanBuildFrom
import scala.collection.{MapLike, mutable}

/**
  * Simple immutable structure to collect named values while retaining their order and
  * providing fast, hashed lookup by name when necessary.
  * Intended to be used for [[multi]] raw parameters.
  * When `caseInsensitive = true`, fetching values by name will be case-insensitive, i.e. keys in internal
  * hashmap and those passed to `contains`, `isDefinedAt`, `apply` and `applyOrElse` will be lowercased.
  */
final class Mapping[+V](private val wrapped: IIterable[(String, V)], caseInsensitive: Boolean = false)
  extends IMap[String, V] with MapLike[String, V, Mapping[V]] {

  private def normKey(key: String): String =
    if (caseInsensitive) key.toLowerCase else key

  private[this] lazy val vector = {
    val keys = new mutable.HashSet[String]
    wrapped.iterator.filter({ case (k, _) => keys.add(normKey(k)) }).toVector
  }
  private[this] lazy val map =
    wrapped.iterator.map({ case (k, v) => (normKey(k), v) }).toMap

  override def empty: Mapping[V] = Mapping.empty
  override protected[this] def newBuilder: mutable.Builder[(String, V), Mapping[V]] =
    Mapping.newBuilder[V]()

  override def size: Int =
    vector.size
  override def iterator: Iterator[(String, V)] =
    vector.iterator
  override def valuesIterator: Iterator[V] =
    vector.iterator.map({ case (_, v) => v })
  override def keys: Iterable[String] =
    vector.map({ case (k, _) => k })
  override def keySet: ISet[String] =
    vector.iterator.map({ case (k, _) => k }).toSet
  override def contains(key: String): Boolean =
    map.contains(normKey(key))
  override def isDefinedAt(key: String): Boolean =
    map.isDefinedAt(normKey(key))
  override def applyOrElse[A1 <: String, B1 >: V](key: A1, default: A1 => B1): B1 =
    if (!caseInsensitive) map.applyOrElse(key, default)
    else map.applyOrElse(normKey(key), (_: String) => default(key))
  override def apply(key: String): V =
    map.apply(normKey(key))

  def get(key: String): Option[V] = map.get(key)
  def -(key: String): Mapping[V] = Mapping(new KeyFilteredIterable(wrapped, _ == key))
  def +[V0 >: V](pair: (String, V0)): Mapping[V0] = append(pair)

  def append[V0 >: V](pair: (String, V0)): Mapping[V0] =
    if (wrapped.isEmpty) Mapping(List(pair))
    else Mapping(new ConcatIterable(wrapped, List(pair)))

  def prepend[V0 >: V](pair: (String, V0)): Mapping[V0] =
    if (wrapped.isEmpty) Mapping(List(pair))
    else Mapping(new ConcatIterable(List(pair), wrapped))

  def ++[V0 >: V](other: Mapping[V0]): Mapping[V0] =
    if (wrapped.isEmpty) other
    else if (other.wrapped.isEmpty) this
    else new Mapping(new ConcatIterable(wrapped, other.wrapped))
}
object Mapping {
  def empty[V]: Mapping[V] = new Mapping(Nil)
  def apply[V](pairs: (String, V)*): Mapping[V] = new Mapping(pairs.toList)
  def apply[V](pairs: IIterable[(String, V)], caseInsensitive: Boolean = false): Mapping[V] =
    new Mapping(pairs, caseInsensitive)

  def newBuilder[V](caseInsensitive: Boolean = false): mutable.Builder[(String, V), Mapping[V]] =
    new MListBuffer[(String, V)].mapResult(new Mapping(_, caseInsensitive))

  private class ConcatIterable[+V](first: IIterable[V], second: IIterable[V]) extends IIterable[V] {
    def iterator: Iterator[V] = first.iterator ++ second.iterator
  }

  private class KeyFilteredIterable[+V](original: IIterable[(String, V)], filter: String => Boolean)
    extends IIterable[(String, V)] {

    def iterator: Iterator[(String, V)] =
      original.iterator.filter { case (k, _) => filter(k) }
  }

  private val reusableCBF = new CanBuildFrom[Nothing, (String, Any), Mapping[Any]] {
    def apply(from: Nothing): mutable.Builder[(String, Any), Mapping[Any]] = newBuilder[Any]()
    def apply(): mutable.Builder[(String, Any), Mapping[Any]] = newBuilder[Any]()
  }

  implicit def canBuildFrom[V]: CanBuildFrom[Nothing, (String, V), Mapping[V]] =
    reusableCBF.asInstanceOf[CanBuildFrom[Nothing, (String, V), Mapping[V]]]

  implicit def genCodec[V: GenCodec]: GenCodec[Mapping[V]] = GenCodec.nullableObject(
    oi => new Mapping(oi.iterator(GenCodec.read[V]).toList),
    (oo, np) => np.foreach({ case (k, v) => GenCodec.write[V](oo.writeField(k), v) })
  )
}
