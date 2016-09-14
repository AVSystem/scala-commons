package com.avsystem.commons
package redis.commands

import akka.util.ByteString
import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion, Opt}
import com.avsystem.commons.redis.CommandEncoder.CommandArg
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.commands.Georadius.{Attributes, NoAttributes}
import com.avsystem.commons.redis.exception.UnexpectedReplyException
import com.avsystem.commons.redis.protocol._

import scala.collection.mutable.ListBuffer

trait GeoApi extends ApiSubset {
  def geoadd(key: ByteString, items: Seq[(ByteString, GeoPoint)]): Result[Long] =
    execute(Geoadd(key, items))
  def geohash(key: ByteString, members: Seq[ByteString]): Result[Seq[Opt[GeohashValue]]] =
    execute(Geohash(key, members))
  def geopos(key: ByteString, members: Seq[ByteString]): Result[Seq[Opt[GeoPoint]]] =
    execute(Geopos(key, members))
  def geodist(key: ByteString, member1: ByteString, member2: ByteString, unit: GeoUnit = GeoUnit.M): Result[Opt[Double]] =
    execute(Geodist(key, member1, member2, unit))
  def georadius[A <: Attributes](key: ByteString, point: GeoPoint, radius: Double, unit: GeoUnit,
    attributes: A = NoAttributes, count: Opt[Long] = Opt.Empty, sortOrder: Opt[SortOrder] = Opt.Empty): Result[Seq[A#Attributed[ByteString]]] =
    execute(Georadius(key, point, radius, unit, attributes, count, sortOrder))
  def georadiusbymember[A <: Attributes](key: ByteString, member: ByteString, radius: Double, unit: GeoUnit,
    attributes: A = NoAttributes, count: Opt[Long] = Opt.Empty, sortOrder: Opt[SortOrder] = Opt.Empty): Result[Seq[A#Attributed[ByteString]]] =
    execute(Georadiusbymember(key, member, radius, unit, attributes, count, sortOrder))
  def georadiusStore(key: ByteString, point: GeoPoint, radius: Double, unit: GeoUnit,
    storeKey: ByteString, storeDist: Boolean = false, count: Opt[Long] = Opt.Empty, sortOrder: Opt[SortOrder] = Opt.Empty): Result[Opt[Long]] =
    execute(GeoradiusStore(key, point, radius, unit, count, sortOrder, storeKey, storeDist))
  def georadiusbymemberStore(key: ByteString, member: ByteString, radius: Double, unit: GeoUnit,
    storeKey: ByteString, storeDist: Boolean = false, count: Opt[Long] = Opt.Empty, sortOrder: Opt[SortOrder] = Opt.Empty): Result[Opt[Long]] =
    execute(GeoradiusbymemberStore(key, member, radius, unit, count, sortOrder, storeKey, storeDist))
}

case class Geoadd(key: ByteString, items: Seq[(ByteString, GeoPoint)]) extends RedisLongCommand with NodeCommand {
  val encoded = encoder("GEOADD").key(key).add(items.iterator.map(_.swap)).result
}

case class Geohash(key: ByteString, members: Seq[ByteString]) extends RedisSeqCommand[Opt[GeohashValue]] with NodeCommand {
  val encoded = encoder("GEOHASH").key(key).add(members).result
  protected val decodeElement: PartialFunction[ValidRedisMsg, Opt[GeohashValue]] = {
    case BulkStringMsg(hash) => GeohashValue(hash.utf8String).opt
    case NullBulkStringMsg => Opt.Empty
  }
}

case class Geopos(key: ByteString, members: Seq[ByteString]) extends RedisSeqCommand[Opt[GeoPoint]] with NodeCommand {
  val encoded = encoder("GEOPOS").key(key).add(members).result
  protected val decodeElement: PartialFunction[ValidRedisMsg, Opt[GeoPoint]] = {
    case ArrayMsg(IndexedSeq(BulkStringMsg(rawLong), BulkStringMsg(rawLat))) =>
      GeoPoint(rawLong.utf8String.toDouble, rawLat.utf8String.toDouble).opt
    case NullArrayMsg => Opt.Empty
  }
}

case class Geodist(key: ByteString, member1: ByteString, member2: ByteString, unit: GeoUnit)
  extends RedisOptDoubleCommand with NodeCommand {
  val encoded = encoder("GEODIST").key(key).add(member1).add(member2).add(unit).result
}

sealed abstract class AbstractGeoradius[T](
  key: ByteString, point: Opt[GeoPoint], member: Opt[ByteString], radius: Double, unit: GeoUnit,
  flags: List[String], count: Opt[Long], sortOrder: Opt[SortOrder], store: Opt[(ByteString, Boolean)])
  extends RedisCommand[T] with NodeCommand {

  val encoded = encoder(if (point.isDefined) "GEORADIUS" else "GEORADIUSBYMEMBER")
    .key(key).add(point).add(member).add(radius).add(unit).add(flags)
    .optAdd("COUNT", count).add(sortOrder)
    .optKey("STORE", store.collect({ case (skey, false) => skey }))
    .optKey("STOREDIST", store.collect({ case (skey, true) => skey }))
    .result

  protected def decodeAttributedElement(attributes: Attributes): PartialFunction[RedisMsg, attributes.Attributed[ByteString]] =
    if (attributes.isEmpty) {
      case BulkStringMsg(mem) =>
        attributes.decode(ArrayMsg.Empty, attributes.flags, mem)
    } else {
      case arr@ArrayMsg(IndexedSeq(BulkStringMsg(mem), _*)) =>
        attributes.decode(arr, attributes.flags, mem)
    }
}

case class Georadius[A <: Attributes](key: ByteString, point: GeoPoint, radius: Double, unit: GeoUnit,
  attributes: A, count: Opt[Long], sortOrder: Opt[SortOrder])
  extends AbstractGeoradius[Seq[A#Attributed[ByteString]]](
    key, point.opt, Opt.Empty, radius, unit, attributes.encodeFlags, count, sortOrder, Opt.Empty)
    with RedisSeqCommand[A#Attributed[ByteString]] {

  protected val decodeElement = decodeAttributedElement(attributes)
}

case class GeoradiusStore(key: ByteString, point: GeoPoint, radius: Double, unit: GeoUnit,
  count: Opt[Long], sortOrder: Opt[SortOrder], storeKey: ByteString, storeDist: Boolean)
  extends AbstractGeoradius[Opt[Long]](
    key, point.opt, Opt.Empty, radius, unit, Nil, count, sortOrder, Opt((storeKey, storeDist)))
    with RedisOptLongCommand

case class Georadiusbymember[A <: Attributes](key: ByteString, member: ByteString, radius: Double, unit: GeoUnit,
  attributes: A, count: Opt[Long], sortOrder: Opt[SortOrder])
  extends AbstractGeoradius[Seq[A#Attributed[ByteString]]](
    key, Opt.Empty, member.opt, radius, unit, attributes.encodeFlags, count, sortOrder, Opt.Empty)
    with RedisSeqCommand[A#Attributed[ByteString]] {

  protected val decodeElement = decodeAttributedElement(attributes)
}

case class GeoradiusbymemberStore(key: ByteString, member: ByteString, radius: Double, unit: GeoUnit,
  count: Opt[Long], sortOrder: Opt[SortOrder], storeKey: ByteString, storeDist: Boolean)
  extends AbstractGeoradius[Opt[Long]](
    key, Opt.Empty, member.opt, radius, unit, Nil, count, sortOrder, Opt((storeKey, storeDist)))
    with RedisOptLongCommand

object Georadius {
  private final val NoFlags = 0
  private final val DistFlag = 1 << 0
  private final val HashFlag = 1 << 1
  private final val CoordFlag = 1 << 2

  private def offset(flags: Int, flag: Int) =
    if ((flags & flag) != 0) 1 else 0

  abstract class Attributes(val flags: Int) { self =>
    type Attributed[A]

    def isEmpty = flags == NoFlags

    def encodeFlags: List[String] = {
      val res = new ListBuffer[String]
      if ((flags & DistFlag) != 0) {
        res += "WITHDIST"
      }
      if ((flags & HashFlag) != 0) {
        res += "WITHHASH"
      }
      if ((flags & CoordFlag) != 0) {
        res += "WITHCOORD"
      }
      res.result()
    }

    def decode[A](element: ArrayMsg[RedisMsg], finalFlags: Int, wrapped: A): Attributed[A]

    def +(other: Attributes): Attributes {type Attributed[A] = self.Attributed[other.Attributed[A]]} =
      new Attributes(self.flags | other.flags) {
        type Attributed[A] = self.Attributed[other.Attributed[A]]
        def decode[A](element: ArrayMsg[RedisMsg], finalFlags: Int, wrapped: A) =
          self.decode(element, finalFlags, other.decode(element, finalFlags, wrapped))
      }
  }

  object NoAttributes extends Attributes(NoFlags) {
    type Attributed[A] = A

    def decode[A](element: ArrayMsg[RedisMsg], finalFlags: Int, wrapped: A) = wrapped
  }

  case class Withdist[A](dist: Double, wrapped: A)
  object Withdist extends Attributes(DistFlag) {
    type Attributed[A] = Withdist[A]

    def decode[A](element: ArrayMsg[RedisMsg], finalFlags: Int, wrapped: A) =
      element.elements(1) match {
        case BulkStringMsg(dist) => Withdist(dist.utf8String.toDouble, wrapped)
        case msg => throw new UnexpectedReplyException(s"Expected bulk string for DIST, got $msg")
      }
  }

  case class Withhash[A](hash: Long, wrapped: A)
  object Withhash extends Attributes(HashFlag) {
    type Attributed[A] = Withhash[A]

    def decode[A](element: ArrayMsg[RedisMsg], finalFlags: Int, wrapped: A) =
      element.elements(1 + offset(finalFlags, DistFlag)) match {
        case IntegerMsg(hash) => Withhash(hash, wrapped)
        case msg => throw new UnexpectedReplyException(s"Expected integer for HASH, got $msg")
      }
  }

  case class Withcoord[A](coords: GeoPoint, wrapped: A)
  object Withcoord extends Attributes(CoordFlag) {
    type Attributed[A] = Withcoord[A]

    def decode[A](element: ArrayMsg[RedisMsg], finalFlags: Int, wrapped: A) =
      element.elements(1 + offset(finalFlags, DistFlag) + offset(finalFlags, HashFlag)) match {
        case ArrayMsg(IndexedSeq(BulkStringMsg(rawLong), BulkStringMsg(rawLat))) =>
          Withcoord(GeoPoint(rawLong.utf8String.toDouble, rawLat.utf8String.toDouble), wrapped)
        case msg => throw new UnexpectedReplyException(s"Expected two-element array of bulk strings for COORD, got $msg")
      }
  }
}

case class GeoPoint(longitude: Double, latitude: Double)
object GeoPoint {
  implicit val commandArg: CommandArg[GeoPoint] = CommandArg {
    case (enc, GeoPoint(long, lat)) =>
      enc.add(long).add(lat)
  }
}

case class GeohashValue(raw: String) extends AnyVal

sealed abstract class GeoUnit(val name: String) extends NamedEnum
object GeoUnit extends NamedEnumCompanion[GeoUnit] {
  case object M extends GeoUnit("m")
  case object Km extends GeoUnit("km")
  case object Mi extends GeoUnit("mi")
  case object Ft extends GeoUnit("ft")

  val values: List[GeoUnit] = caseObjects
}
