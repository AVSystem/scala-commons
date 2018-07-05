package com.avsystem.commons
package rpc

import com.avsystem.commons.rpc.NamedParams.ConcatIterable
import com.avsystem.commons.serialization.GenCodec

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

/**
  * Simple immutable structure to collect named RPC parameters while retaining their order and
  * providing fast, hashed lookup by parameter name when necessary.
  * Intended to be used for [[multi]] raw parameters.
  */
final class NamedParams[+V](private val wrapped: IIterable[(String, V)])
  extends IIterable[(String, V)] with PartialFunction[String, V] {

  private[this] lazy val hashMap = new MHashMap[String, V].setup(_ ++= wrapped)

  def iterator: Iterator[(String, V)] =
    hashMap.iterator
  def isDefinedAt(key: String): Boolean =
    hashMap.isDefinedAt(key)
  override def applyOrElse[A1 <: String, B1 >: V](key: A1, default: A1 => B1): B1 =
    hashMap.applyOrElse(key, default)
  override def apply(key: String): V =
    hashMap.apply(key)

  def ++[V0 >: V](other: NamedParams[V0]): NamedParams[V0] =
    if (wrapped.isEmpty) other
    else if (other.wrapped.isEmpty) this
    else new NamedParams(ConcatIterable(wrapped, other.wrapped))
}
object NamedParams {
  def empty[V]: NamedParams[V] = new NamedParams(Nil)
  def newBuilder[V]: mutable.Builder[(String, V), NamedParams[V]] =
    new MListBuffer[(String, V)].mapResult(new NamedParams(_))

  private case class ConcatIterable[+V](first: IIterable[V], second: IIterable[V]) extends IIterable[V] {
    def iterator: Iterator[V] = first.iterator ++ second.iterator
  }

  private val reusableCBF = new CanBuildFrom[Nothing, (String, Any), NamedParams[Any]] {
    def apply(from: Nothing): mutable.Builder[(String, Any), NamedParams[Any]] = newBuilder[Any]
    def apply(): mutable.Builder[(String, Any), NamedParams[Any]] = newBuilder[Any]
  }

  implicit def canBuildFrom[V]: CanBuildFrom[Nothing, (String, V), NamedParams[V]] =
    reusableCBF.asInstanceOf[CanBuildFrom[Nothing, (String, V), NamedParams[V]]]

  implicit def genCodec[V: GenCodec]: GenCodec[NamedParams[V]] = GenCodec.createNullableObject(
    oi => new NamedParams(oi.iterator(GenCodec.read[V]).toList),
    (oo, np) => np.foreach({ case (k, v) => GenCodec.write[V](oo.writeField(k), v) })
  )
}
