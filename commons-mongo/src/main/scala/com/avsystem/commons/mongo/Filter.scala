package com.avsystem.commons
package mongo

import com.avsystem.commons.jiop.JavaInterop._
import com.mongodb.client.model.{Filters => F}
import org.bson._
import org.bson.conversions.Bson

/**
  * @author MKej
  */
object Filter {

  import Limitations._

  def and(filters: Bson*): Bson = F.and(filters.asJava)

  def eq[A](key: DocKey[A, _], value: A): Bson = F.eq(key.key, key.codec.toBson(value))
  def ne[A](key: DocKey[A, _], value: A): Bson = F.ne(key.key, key.codec.toBson(value))

  def lt[A, BSON <: BsonValue : CanCompare](key: DocKey[A, BSON], value: A): Bson = F.lt(key.key, key.codec.toBson(value))

  def lte[A, BSON <: BsonValue : CanCompare](key: DocKey[A, BSON], value: A): Bson = F.lte(key.key, key.codec.toBson(value))

  def gt[A, BSON <: BsonValue : CanCompare](key: DocKey[A, BSON], value: A): Bson = F.gt(key.key, key.codec.toBson(value))

  def gte[A, BSON <: BsonValue : CanCompare](key: DocKey[A, BSON], value: A): Bson = F.gte(key.key, key.codec.toBson(value))

  def in[A](key: DocKey[A, _], values: Iterable[A]): Bson = F.in(key.key, values.map(key.codec.toBson))

  def nin[A](key: DocKey[A, _], values: Iterable[A]): Bson = F.nin(key.key, values.map(key.codec.toBson))

  def regex[A](key: DocKey[A, BsonString], pattern: String): Bson = F.regex(key.key, pattern)

  def exists(key: DocKey[_, _]): Bson = F.exists(key.key, true)

  def notExists(key: DocKey[_, _]): Bson = F.exists(key.key, false)

  private object Limitations {
    trait CanCompare[BSON <: BsonValue]
    object CanCompare {
      def create[BSON <: BsonValue]: CanCompare[BSON] = new CanCompare[BSON] {}
      implicit val date = create[BsonDateTime]
      implicit val int32 = create[BsonInt32]
      implicit val int64 = create[BsonInt64]
      implicit val double = create[BsonDouble]
    }
  }
}
