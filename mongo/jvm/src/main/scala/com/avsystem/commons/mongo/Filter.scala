package com.avsystem.commons
package mongo

import com.mongodb.client.model.{Filters => F}
import org.bson.conversions.Bson
import org.bson.{BsonArray, BsonDateTime, BsonDouble, BsonInt32, BsonInt64, BsonString, BsonValue}

import _root_.scala.collection.compat._

/**
  * @author MKej
  */
object Filter {

  import Limitations._

  def and(filters: Bson*): Bson = F.and(filters.asJava)
  def or(filters: Bson*): Bson = F.or(filters.asJava)

  @deprecated(message = "Use `equal` instead", since = "1.19.9")
  def eq[A](key: DocKey[A, _], value: A): Bson = equal(key, value)

  @deprecated(message = "Use `notEqual` instead", since = "1.19.9")
  def ne[A](key: DocKey[A, _], value: A): Bson = notEqual(key, value)

  def equal[A](key: DocKey[A, _], value: A): Bson = F.eq(key.key, key.codec.toBson(value))
  def notEqual[A](key: DocKey[A, _], value: A): Bson = F.ne(key.key, key.codec.toBson(value))

  def lt[A, BSON <: BsonValue : CanCompare](key: DocKey[A, BSON], value: A): Bson = F.lt(key.key, key.codec.toBson(value))

  def lte[A, BSON <: BsonValue : CanCompare](key: DocKey[A, BSON], value: A): Bson = F.lte(key.key, key.codec.toBson(value))

  def gt[A, BSON <: BsonValue : CanCompare](key: DocKey[A, BSON], value: A): Bson = F.gt(key.key, key.codec.toBson(value))

  def gte[A, BSON <: BsonValue : CanCompare](key: DocKey[A, BSON], value: A): Bson = F.gte(key.key, key.codec.toBson(value))

  def in[A](key: DocKey[A, _], values: Iterable[A]): Bson = F.in(key.key, values.map(key.codec.toBson).asJava)

  def nin[A](key: DocKey[A, _], values: Iterable[A]): Bson = F.nin(key.key, values.map(key.codec.toBson).asJava)

  def regex[A](key: DocKey[A, BsonString], pattern: String): Bson = F.regex(key.key, pattern)

  def exists(key: DocKey[_, _]): Bson = F.exists(key.key, true)

  def notExists(key: DocKey[_, _]): Bson = F.exists(key.key, false)

  def elemMatch(key: DocKey[_, _ <: BsonArray], filter: Bson): Bson = F.elemMatch(key.key, filter)

  def contains[A, COL <: Iterable[A]](key: DocKey[COL, _ <: BsonArray], value: A)
    (implicit fac: Factory[A, COL]): Bson =
    F.eq(key.key, key.codec.toBson((fac.newBuilder += value).result()).asScala.head)

  object Limitations {
    trait CanCompare[BSON <: BsonValue]
    object CanCompare {
      def create[BSON <: BsonValue]: CanCompare[BSON] = new CanCompare[BSON] {}
      implicit val date: CanCompare[BsonDateTime] = create[BsonDateTime]
      implicit val int32: CanCompare[BsonInt32] = create[BsonInt32]
      implicit val int64: CanCompare[BsonInt64] = create[BsonInt64]
      implicit val double: CanCompare[BsonDouble] = create[BsonDouble]
    }
  }
}
