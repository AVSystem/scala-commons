package com.avsystem.commons
package redis.commands

import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion, Opt}
import com.avsystem.commons.redis.CommandEncoder.CommandArg
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.exception.UnexpectedReplyException
import com.avsystem.commons.redis.protocol._

import scala.collection.mutable.ListBuffer

trait GeoApi extends ApiSubset {
  def geoadd(key: Key, items: Seq[(Value, GeoPoint)]): Result[Long] =
    execute(Geoadd(key, items))
  def geohash(key: Key, members: Seq[Value]): Result[Seq[Opt[GeohashValue]]] =
    execute(Geohash(key, members))
  def geopos(key: Key, members: Seq[Value]): Result[Seq[Opt[GeoPoint]]] =
    execute(Geopos(key, members))
  def geodist(key: Key, member1: Value, member2: Value, unit: GeoUnit = GeoUnit.M): Result[Opt[Double]] =
    execute(Geodist(key, member1, member2, unit))
  def georadius[A <: GeoradiusAttrs](key: Key, point: GeoPoint, radius: Double, unit: GeoUnit,
    attributes: A = GeoradiusAttrs.None, count: Opt[Long] = Opt.Empty, sortOrder: Opt[SortOrder] = Opt.Empty): Result[Seq[A#Attributed[Value]]] =
    execute(Georadius(key, point, radius, unit, attributes, count, sortOrder))
  def georadiusbymember[A <: GeoradiusAttrs](key: Key, member: Value, radius: Double, unit: GeoUnit,
    attributes: A = GeoradiusAttrs.None, count: Opt[Long] = Opt.Empty, sortOrder: Opt[SortOrder] = Opt.Empty): Result[Seq[A#Attributed[Value]]] =
    execute(Georadiusbymember(key, member, radius, unit, attributes, count, sortOrder))
  def georadiusStore(key: Key, point: GeoPoint, radius: Double, unit: GeoUnit,
    storeKey: Key, storeDist: Boolean = false, count: Opt[Long] = Opt.Empty, sortOrder: Opt[SortOrder] = Opt.Empty): Result[Opt[Long]] =
    execute(GeoradiusStore(key, point, radius, unit, count, sortOrder, storeKey, storeDist))
  def georadiusbymemberStore(key: Key, member: Value, radius: Double, unit: GeoUnit,
    storeKey: Key, storeDist: Boolean = false, count: Opt[Long] = Opt.Empty, sortOrder: Opt[SortOrder] = Opt.Empty): Result[Opt[Long]] =
    execute(GeoradiusbymemberStore(key, member, radius, unit, count, sortOrder, storeKey, storeDist))

  case class Geoadd(key: Key, items: Seq[(Value, GeoPoint)]) extends RedisLongCommand with NodeCommand {
    val encoded = encoder("GEOADD").key(key).add(items.iterator.map({case (v, p) => (p, valueCodec.write(v))})).result
  }

  case class Geohash(key: Key, members: Seq[Value]) extends RedisSeqCommand[Opt[GeohashValue]] with NodeCommand {
    val encoded = encoder("GEOHASH").key(key).values(members).result
    protected val decodeElement: PartialFunction[ValidRedisMsg, Opt[GeohashValue]] = {
      case BulkStringMsg(hash) => GeohashValue(hash.utf8String).opt
      case NullBulkStringMsg => Opt.Empty
    }
  }

  case class Geopos(key: Key, members: Seq[Value]) extends RedisSeqCommand[Opt[GeoPoint]] with NodeCommand {
    val encoded = encoder("GEOPOS").key(key).values(members).result
    protected val decodeElement: PartialFunction[ValidRedisMsg, Opt[GeoPoint]] = {
      case ArrayMsg(IndexedSeq(BulkStringMsg(rawLong), BulkStringMsg(rawLat))) =>
        GeoPoint(rawLong.utf8String.toDouble, rawLat.utf8String.toDouble).opt
      case NullArrayMsg => Opt.Empty
    }
  }

  case class Geodist(key: Key, member1: Value, member2: Value, unit: GeoUnit)
    extends RedisOptDoubleCommand with NodeCommand {
    val encoded = encoder("GEODIST").key(key).value(member1).value(member2).add(unit).result
  }

  sealed abstract class AbstractGeoradius[T](
    key: Key, point: Opt[GeoPoint], member: Opt[Value], radius: Double, unit: GeoUnit,
    flags: List[String], count: Opt[Long], sortOrder: Opt[SortOrder], store: Opt[(Key, Boolean)])
    extends RedisCommand[T] with NodeCommand {

    val encoded = encoder(if (point.isDefined) "GEORADIUS" else "GEORADIUSBYMEMBER")
      .key(key).add(point).add(member.map(valueCodec.write)).add(radius).add(unit).add(flags)
      .optAdd("COUNT", count).add(sortOrder)
      .optKey("STORE", store.collect({ case (skey, false) => skey }))
      .optKey("STOREDIST", store.collect({ case (skey, true) => skey }))
      .result

    protected def decodeAttributedElement(attributes: GeoradiusAttrs): PartialFunction[RedisMsg, attributes.Attributed[Value]] =
      if (attributes.isEmpty) {
        case BulkStringMsg(mem) =>
          attributes.decode(ArrayMsg.Empty, attributes.flags, valueCodec.read(mem))
      } else {
        case arr@ArrayMsg(IndexedSeq(BulkStringMsg(mem), _*)) =>
          attributes.decode(arr, attributes.flags, valueCodec.read(mem))
      }
  }

  case class Georadius[A <: GeoradiusAttrs](key: Key, point: GeoPoint, radius: Double, unit: GeoUnit,
    attributes: A, count: Opt[Long], sortOrder: Opt[SortOrder])
    extends AbstractGeoradius[Seq[A#Attributed[Value]]](
      key, point.opt, Opt.Empty, radius, unit, attributes.encodeFlags, count, sortOrder, Opt.Empty)
      with RedisSeqCommand[A#Attributed[Value]] {

    protected val decodeElement = decodeAttributedElement(attributes)
  }

  case class GeoradiusStore(key: Key, point: GeoPoint, radius: Double, unit: GeoUnit,
    count: Opt[Long], sortOrder: Opt[SortOrder], storeKey: Key, storeDist: Boolean)
    extends AbstractGeoradius[Opt[Long]](
      key, point.opt, Opt.Empty, radius, unit, Nil, count, sortOrder, Opt((storeKey, storeDist)))
      with RedisOptLongCommand

  case class Georadiusbymember[A <: GeoradiusAttrs](key: Key, member: Value, radius: Double, unit: GeoUnit,
    attributes: A, count: Opt[Long], sortOrder: Opt[SortOrder])
    extends AbstractGeoradius[Seq[A#Attributed[Value]]](
      key, Opt.Empty, member.opt, radius, unit, attributes.encodeFlags, count, sortOrder, Opt.Empty)
      with RedisSeqCommand[A#Attributed[Value]] {

    protected val decodeElement = decodeAttributedElement(attributes)
  }

  case class GeoradiusbymemberStore(key: Key, member: Value, radius: Double, unit: GeoUnit,
    count: Opt[Long], sortOrder: Opt[SortOrder], storeKey: Key, storeDist: Boolean)
    extends AbstractGeoradius[Opt[Long]](
      key, Opt.Empty, member.opt, radius, unit, Nil, count, sortOrder, Opt((storeKey, storeDist)))
      with RedisOptLongCommand
}

abstract class GeoradiusAttrs(val flags: Int) { self =>
  import GeoradiusAttrs._

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

  def +(other: GeoradiusAttrs): GeoradiusAttrs {type Attributed[A] = self.Attributed[other.Attributed[A]]} =
    new GeoradiusAttrs(self.flags | other.flags) {
      type Attributed[A] = self.Attributed[other.Attributed[A]]
      def decode[A](element: ArrayMsg[RedisMsg], finalFlags: Int, wrapped: A) =
        self.decode(element, finalFlags, other.decode(element, finalFlags, wrapped))
    }
}
object GeoradiusAttrs {
  private final val NoFlags = 0
  private final val DistFlag = 1 << 0
  private final val HashFlag = 1 << 1
  private final val CoordFlag = 1 << 2

  private def offset(flags: Int, flag: Int) =
    if ((flags & flag) != 0) 1 else 0

  object None extends GeoradiusAttrs(NoFlags) {
    type Attributed[A] = A

    def decode[A](element: ArrayMsg[RedisMsg], finalFlags: Int, wrapped: A) = wrapped
  }

  case class Withdist[A](dist: Double, wrapped: A)
  object Withdist extends GeoradiusAttrs(DistFlag) {
    type Attributed[A] = Withdist[A]

    def decode[A](element: ArrayMsg[RedisMsg], finalFlags: Int, wrapped: A) =
      element.elements(1) match {
        case BulkStringMsg(dist) => Withdist(dist.utf8String.toDouble, wrapped)
        case msg => throw new UnexpectedReplyException(s"Expected bulk string for DIST, got $msg")
      }
  }

  case class Withhash[A](hash: Long, wrapped: A)
  object Withhash extends GeoradiusAttrs(HashFlag) {
    type Attributed[A] = Withhash[A]

    def decode[A](element: ArrayMsg[RedisMsg], finalFlags: Int, wrapped: A) =
      element.elements(1 + offset(finalFlags, DistFlag)) match {
        case IntegerMsg(hash) => Withhash(hash, wrapped)
        case msg => throw new UnexpectedReplyException(s"Expected integer for HASH, got $msg")
      }
  }

  case class Withcoord[A](coords: GeoPoint, wrapped: A)
  object Withcoord extends GeoradiusAttrs(CoordFlag) {
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
