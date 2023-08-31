package com.avsystem.commons
package mongo

import org.bson.types.ObjectId
import org.bson._

import java.time.Instant
import _root_.scala.collection.Factory
import _root_.scala.language.higherKinds

/**
  * @author MKej
  */
trait BsonCodec[A, BSON <: BsonValue] {self =>
  def fromBson(bson: BSON): A
  def toBson(a: A): BSON

  def map[B](fa: A => B, fb: B => A): BsonCodec[B, BSON] = new BsonCodec[B, BSON] {
    def fromBson(bson: BSON) = fa(self.fromBson(bson))
    def toBson(b: B) = self.toBson(fb(b))
  }

  def key(key: String): DocKey[A, BSON] = new DocKey[A, BSON](key, this)

  def collection[C[X] <: IterableOnce[X]](implicit fac: Factory[A, C[A]]): BsonCodec[C[A], BsonArray] =
    BsonCodec.create[C[A], BsonArray](
      ba => ba.iterator().asScala.map(bv => self.fromBson(bv.asInstanceOf[BSON])).to(fac),
      col => new BsonArray(col.iterator.map(self.toBson).to(JList))
    )
}

object BsonCodec {
  def create[A, BSON <: BsonValue](from: BSON => A, to: A => BSON): BsonCodec[A, BSON] = new BsonCodec[A, BSON] {
    def fromBson(bson: BSON) = from(bson)
    def toBson(a: A) = to(a)
  }

  private object _identity extends BsonCodec[BsonValue, BsonValue] {
    override def fromBson(bson: BsonValue) = bson
    override def toBson(a: BsonValue) = a
  }
  def identity[B <: BsonValue] = _identity.asInstanceOf[BsonCodec[B, B]]

  val objectId = create[ObjectId, BsonObjectId](_.getValue, new BsonObjectId(_))

  val byteArray = create[Array[Byte], BsonBinary](_.getData, new BsonBinary(_))

  val boolean = create[Boolean, BsonBoolean](_.getValue, new BsonBoolean(_))
  val int32 = create[Int, BsonInt32](_.getValue, new BsonInt32(_))
  val int64 = create[Long, BsonInt64](_.getValue, new BsonInt64(_))
  val double = create[Double, BsonDouble](_.getValue, new BsonDouble(_))

  val string = create[String, BsonString](_.getValue, new BsonString(_))

  val doc = create[Doc, BsonDocument](new Doc(_), _.toBson)
  val instant = create[Instant, BsonDateTime](
    bdt => Instant.ofEpochMilli(bdt.getValue),
    i => new BsonDateTime(i.toEpochMilli)
  )
}
