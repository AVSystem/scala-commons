package com.avsystem.commons
package meta

import com.avsystem.commons.meta.Mapping.ConcatIterable
import com.avsystem.commons.serialization.GenCodec

import scala.collection.generic.CanBuildFrom
import scala.collection.{IterableLike, mutable}

/**
  * Simple immutable structure to collect named values while retaining their order and
  * providing fast, hashed lookup by name when necessary.
  * Intended to be used for [[multi]] raw parameters.
  */
final class Mapping[+V](private val wrapped: IIterable[(String, V)])
  extends IIterable[(String, V)] with IterableLike[(String, V), Mapping[V]] with PartialFunction[String, V] {

  private[this] lazy val hashMap = new MLinkedHashMap[String, V].setup(_ ++= wrapped)

  override protected[this] def newBuilder: mutable.Builder[(String, V), Mapping[V]] =
    Mapping.newBuilder[V]

  def iterator: Iterator[(String, V)] =
    hashMap.iterator
  def valuesIterator: Iterator[V] =
    hashMap.valuesIterator
  def keys: Iterable[String] =
    hashMap.keys
  def contains(key: String): Boolean =
    hashMap.contains(key)
  def isDefinedAt(key: String): Boolean =
    hashMap.isDefinedAt(key)
  override def applyOrElse[A1 <: String, B1 >: V](key: A1, default: A1 => B1): B1 =
    hashMap.applyOrElse(key, default)
  override def apply(key: String): V =
    hashMap.apply(key)
  def get(key: String): Opt[V] =
    hashMap.getOpt(key)
  def asMap: BMap[String, V] =
    hashMap

  def ++[V0 >: V](other: Mapping[V0]): Mapping[V0] =
    if (wrapped.isEmpty) other
    else if (other.wrapped.isEmpty) this
    else new Mapping(ConcatIterable(wrapped, other.wrapped))
}
object Mapping {
  def empty[V]: Mapping[V] = new Mapping(Nil)
  def apply[V](pairs: (String, V)*): Mapping[V] = new Mapping(pairs.toList)

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
