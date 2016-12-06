package com.avsystem.commons
package redis.commands

import akka.util.ByteString
import com.avsystem.commons.collection.CollectionAliases.{BMap, BSet, MHashSet}
import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion, Opt}
import com.avsystem.commons.redis.exception.{ErrorReplyException, UnexpectedReplyException}
import com.avsystem.commons.redis.protocol._
import com.avsystem.commons.redis.{NodeAddress, RedisDataCodec}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object `package` {
  type ReplyDecoder[+T] = PartialFunction[ValidRedisMsg, T]
  type ReplyPairDecoder[+T] = PartialFunction[(ValidRedisMsg, ValidRedisMsg), T]
}

object ReplyDecoders {
  val failing: ReplyDecoder[Nothing] = {
    case msg => throw new UnexpectedReplyException(s"Expected no reply, got $msg")
  }

  val undecoded: ReplyDecoder[ValidRedisMsg] = {
    case msg => msg
  }

  val simpleOkUnit: ReplyDecoder[Unit] = {
    case RedisMsg.Ok => ()
  }

  val nullBulkOrSimpleOkBoolean: ReplyDecoder[Boolean] = {
    case RedisMsg.Ok => true
    case NullBulkStringMsg => false
  }

  val integerLong: ReplyDecoder[Long] = {
    case IntegerMsg(value) => value
  }

  val positiveIntegerLongOpt: ReplyDecoder[Opt[Long]] = {
    case IntegerMsg(value) => if (value > 0) value.opt else Opt.Empty
  }

  val integerInt: ReplyDecoder[Int] = {
    case IntegerMsg(value) => value.toInt
  }

  val integerBoolean: ReplyDecoder[Boolean] = {
    case IntegerMsg(0) => false
    case IntegerMsg(1) => true
  }

  val integerTtl: ReplyDecoder[Opt[Opt[Long]]] = {
    case IntegerMsg(-2) => Opt.Empty
    case IntegerMsg(-1) => Opt(Opt.Empty)
    case IntegerMsg(ttl) => Opt(Opt(ttl))
  }

  def bulkNamedEnum[E <: NamedEnum](companion: NamedEnumCompanion[E]): ReplyDecoder[E] = {
    case BulkStringMsg(data) => companion.byName(data.utf8String)
  }

  val bulkLong: ReplyDecoder[Long] =
    bulk(_.utf8String.toLong)

  val bulkInt: ReplyDecoder[Long] =
    bulk(_.utf8String.toInt)

  val bulkDouble: ReplyDecoder[Double] =
    bulk(_.utf8String.toDouble)

  val bulkUTF8: ReplyDecoder[String] =
    bulk(_.utf8String)

  val bulkBinary: ReplyDecoder[ByteString] =
    bulk(bs => bs)

  val bulkSha1: ReplyDecoder[Sha1] =
    bulk(bs => Sha1(bs.utf8String))

  val bulkNodeId: ReplyDecoder[NodeId] =
    bulk(bs => NodeId(bs.utf8String))

  val bulkClientInfos: ReplyDecoder[Seq[ClientInfo]] = {
    case BulkStringMsg(data) =>
      Source.fromInputStream(data.iterator.asInputStream).getLines()
        .map(_.trim).filter(_.nonEmpty).map(line => ClientInfo(line)).to[ArrayBuffer]
  }

  val bulkNodeInfos: ReplyDecoder[Seq[NodeInfo]] = {
    case BulkStringMsg(nodeInfos) =>
      nodeInfos.utf8String.split("\n").iterator.filter(_.nonEmpty).map(NodeInfo).toIndexedSeq
  }

  val bulkCursor: ReplyDecoder[Cursor] = {
    case BulkStringMsg(data) => Cursor(data.utf8String.toLong)
  }

  val simpleUTF8: ReplyDecoder[String] =
    simple(_.utf8String)

  val simpleBinary: ReplyDecoder[ByteString] =
    simple(bs => bs)

  def simple[T](fun: ByteString => T): ReplyDecoder[T] = {
    case SimpleStringMsg(data) => fun(data)
  }

  def simple[T: RedisDataCodec]: ReplyDecoder[T] =
    simple(RedisDataCodec[T].read)

  def bulk[T](fun: ByteString => T): ReplyDecoder[T] = {
    case BulkStringMsg(data) => fun(data)
  }

  def bulk[T: RedisDataCodec]: ReplyDecoder[T] =
    bulk(RedisDataCodec[T].read)

  def nullBulkOr[T](decoder: ReplyDecoder[T]): ReplyDecoder[Opt[T]] =
    decoder.andThen(_.opt) unless {
      case NullBulkStringMsg => Opt.Empty
    }

  def nullBulkOr[T: RedisDataCodec]: ReplyDecoder[Opt[T]] =
    nullBulkOr(bulk[T])

  def nullOrEmptyBulkOr[T](decoder: ReplyDecoder[T]): ReplyDecoder[Opt[T]] =
    decoder.andThen(_.opt) unless {
      case NullBulkStringMsg | BulkStringMsg(ByteString.empty) => Opt.Empty
    }

  def nullOrEmptyBulkOr[T](fun: ByteString => T): ReplyDecoder[Opt[T]] =
    nullOrEmptyBulkOr(bulk(fun))

  def nullOrEmptyBulkOr[T: RedisDataCodec]: ReplyDecoder[Opt[T]] =
    nullOrEmptyBulkOr(RedisDataCodec[T].read)

  private def multiBulkIterator[T](elements: Seq[RedisMsg], elementDecoder: ReplyDecoder[T]): Iterator[T] =
    elements.iterator.map {
      case vrm: ValidRedisMsg => elementDecoder.applyOrElse(vrm, (_: ValidRedisMsg) =>
        throw new UnexpectedReplyException(s"Unexpected element in multi-bulk reply: $vrm"))
      case err: ErrorMsg => throw new ErrorReplyException(err)
    }

  def multiBulkSeq[T](elementDecoder: ReplyDecoder[T]): ReplyDecoder[Seq[T]] = {
    case ArrayMsg(elements) => multiBulkIterator(elements, elementDecoder).to[ArrayBuffer]
  }

  def multiBulkSeq[T: RedisDataCodec]: ReplyDecoder[Seq[T]] =
    multiBulkSeq(bulk[T])

  def multiBulkSet[T](elementDecoder: ReplyDecoder[T]): ReplyDecoder[BSet[T]] = {
    case ArrayMsg(elements) => multiBulkIterator(elements, elementDecoder).to[MHashSet]
  }

  def multiBulkSet[T: RedisDataCodec]: ReplyDecoder[BSet[T]] =
    multiBulkSet(bulk[T])

  def multiBulkPair[A, B](firstDecoder: ReplyDecoder[A], secondDecoder: ReplyDecoder[B]): ReplyDecoder[(A, B)] = {
    case ArrayMsg(IndexedSeq(f: ValidRedisMsg, s: ValidRedisMsg)) =>
      val first = firstDecoder.applyOrElse(f, (_: ValidRedisMsg) =>
        throw new UnexpectedReplyException(s"Unexpected first element in multi-bulk reply: $f"))
      val second = secondDecoder.applyOrElse(s, (_: ValidRedisMsg) =>
        throw new UnexpectedReplyException(s"Unexpected second element in multi-bulk reply: $s"))
      (first, second)
  }

  val multiBulkGeoPoint: ReplyDecoder[GeoPoint] = {
    case ArrayMsg(IndexedSeq(BulkStringMsg(rawLong), BulkStringMsg(rawLat))) =>
      GeoPoint(rawLong.utf8String.toDouble, rawLat.utf8String.toDouble)
  }

  val multiBulkCommandInfo: ReplyDecoder[CommandInfo] = {
    case ArrayMsg(IndexedSeq(BulkStringMsg(name), IntegerMsg(arity), ArrayMsg(flagArray), IntegerMsg(firstKey), IntegerMsg(lastKey), IntegerMsg(stepCount))) =>
      val flags = flagArray.iterator.map({
        case SimpleStringMsg(flagStr) => CommandFlags.byRepr(flagStr.utf8String)
        case msg => throw new UnexpectedReplyException(s"Expected only simple strings in command flag list, got $msg")
      }).fold(CommandFlags.NoFlags)(_ | _)
      CommandInfo(name.utf8String, CommandArity(math.abs(arity.toInt), arity < 0), flags, firstKey.toInt, lastKey.toInt, stepCount.toInt)
  }

  val multiBulkRedisRole: ReplyDecoder[RedisRole] = {
    case ArrayMsg(IndexedSeq(RedisRole.MasterStr, IntegerMsg(replOffset), ArrayMsg(rawSlaveOffsets))) =>
      val slaveOffsets = rawSlaveOffsets.map {
        case ArrayMsg(IndexedSeq(BulkStringMsg(ip), BulkStringMsg(port), BulkStringMsg(offset))) =>
          (NodeAddress(ip.utf8String, port.utf8String.toInt), offset.utf8String.toLong)
        case el => throw new UnexpectedReplyException(s"Unexpected message for slave info: $el")
      }
      MasterRole(replOffset, slaveOffsets)
    case ArrayMsg(IndexedSeq(RedisRole.SlaveStr, BulkStringMsg(ip), IntegerMsg(port), BulkStringMsg(replState), IntegerMsg(dataReceivedOffset))) =>
      SlaveRole(NodeAddress(ip.utf8String, port.toInt), ReplState.byName(replState.utf8String), dataReceivedOffset)
    case ArrayMsg(IndexedSeq(RedisRole.SentinelStr, ArrayMsg(rawMasterNames))) =>
      val masterNames = rawMasterNames.map {
        case BulkStringMsg(masterName) => masterName.utf8String
        case el => throw new UnexpectedReplyException(s"Unexpected message for master name: $el")
      }
      SentinelRole(masterNames)
  }

  val multiBulkSlowlogEntry: ReplyDecoder[SlowlogEntry] = {
    case ArrayMsg(IndexedSeq(IntegerMsg(id), IntegerMsg(timestamp), IntegerMsg(duration), ArrayMsg(rawCommand))) =>
      val commandArgs = rawCommand.map {
        case BulkStringMsg(arg) => arg
        case el => throw new UnexpectedReplyException(s"Unexpected message for SLOWLOG command argument: $el")
      }
      SlowlogEntry(id, timestamp, duration, commandArgs)
  }

  val multiBulkRedisTimestamp: ReplyDecoder[RedisTimestamp] = {
    case ArrayMsg(IndexedSeq(BulkStringMsg(seconds), BulkStringMsg(useconds))) =>
      RedisTimestamp(seconds.utf8String.toLong, useconds.utf8String.toLong)
  }

  def multiBulkSlotRangeMapping[N](nodeFormat: SlotsNodeFormat[N]): ReplyDecoder[SlotRangeMapping[N]] = {
    case ArrayMsg(IndexedSeq(IntegerMsg(from), IntegerMsg(to), master, slaves@_*)) =>
      val range = SlotRange(from.toInt, to.toInt)
      def parseNode(rr: RedisMsg) = rr match {
        case arr: ArrayMsg[RedisMsg] => nodeFormat.parseNode
          .applyOrElse(arr, (_: ArrayMsg[RedisMsg]) => throw new UnexpectedReplyException(s"bad entry in CLUSTER SLOTS reply: $arr"))
        case msg =>
          throw new UnexpectedReplyException(s"bad entry in CLUSTER SLOTS reply: $rr")
      }
      SlotRangeMapping(range, parseNode(master), slaves.map(parseNode))
  }

  def groupedMultiBulk[T](size: Int, elementDecoder: ReplyDecoder[T]): ReplyDecoder[Seq[Seq[T]]] = {
    case ArrayMsg(elements) =>
      def elemDecode(msg: RedisMsg): T = msg match {
        case vrm: ValidRedisMsg => elementDecoder.applyOrElse(vrm, (_: ValidRedisMsg) => throw new UnexpectedReplyException(vrm.toString))
        case err: ErrorMsg => throw new ErrorReplyException(err)
      }
      elements.iterator.grouped(size).map(_.iterator.map(elemDecode).to[ArrayBuffer]).to[ArrayBuffer]
  }

  def nullMultiBulkOr[T](decoder: ReplyDecoder[T]): ReplyDecoder[Opt[T]] =
    decoder.andThen(_.opt) unless {
      case NullArrayMsg => Opt.Empty
    }

  def nullMultiBulkOr[T: RedisDataCodec]: ReplyDecoder[Opt[T]] =
    nullMultiBulkOr(bulk[T])

  def pairedMultiBulk[T](pairDecoder: ReplyPairDecoder[T]): ReplyDecoder[Seq[T]] = {
    case ArrayMsg(elements) => elements.iterator.grouped(2).map {
      case Seq(first: ValidRedisMsg, second: ValidRedisMsg) => pairDecoder.applyOrElse((first, second),
        (p: (ValidRedisMsg, ValidRedisMsg)) => throw new UnexpectedReplyException(s"Unexpected element pair in multi-bulk reply: $p"))
    }.to[ArrayBuffer]
  }

  private def pairedMultiBulkIterator[A, B](elements: Seq[RedisMsg], firstDecoder: ReplyDecoder[A], secondDecoder: ReplyDecoder[B]): Iterator[(A, B)] =
    elements.iterator.grouped(2).map {
      case Seq(f: ValidRedisMsg, s: ValidRedisMsg) =>
        val first = firstDecoder.applyOrElse(f, (_: ValidRedisMsg) =>
          throw new UnexpectedReplyException(s"Unexpected element in multi-bulk reply: $f"))
        val second = secondDecoder.applyOrElse(s, (_: ValidRedisMsg) =>
          throw new UnexpectedReplyException(s"Unexpected element in multi-bulk reply: $s"))
        (first, second)
    }

  def pairedMultiBulk[A, B](firstDecoder: ReplyDecoder[A], secondDecoder: ReplyDecoder[B]): ReplyDecoder[Seq[(A, B)]] = {
    case ArrayMsg(elements) => pairedMultiBulkIterator(elements, firstDecoder, secondDecoder).to[ArrayBuffer]
  }

  def reversePairedMultiBulk[A, B](firstDecoder: ReplyDecoder[A], secondDecoder: ReplyDecoder[B]): ReplyDecoder[Seq[(B, A)]] = {
    case ArrayMsg(elements) => pairedMultiBulkIterator(elements, firstDecoder, secondDecoder).map(_.swap).to[ArrayBuffer]
  }

  def mapMultiBulk[A, B](firstDecoder: ReplyDecoder[A], secondDecoder: ReplyDecoder[B]): ReplyDecoder[BMap[A, B]] = {
    case ArrayMsg(elements) => new mutable.OpenHashMap() ++ pairedMultiBulkIterator(elements, firstDecoder, secondDecoder)
  }

  def pairedMultiBulk[A: RedisDataCodec, B: RedisDataCodec]: ReplyDecoder[Seq[(A, B)]] =
    pairedMultiBulk(bulk(RedisDataCodec[A].read), bulk(RedisDataCodec[B].read))

  def mapMultiBulk[A: RedisDataCodec, B: RedisDataCodec]: ReplyDecoder[BMap[A, B]] =
    mapMultiBulk(bulk(RedisDataCodec[A].read), bulk(RedisDataCodec[B].read))

  def geoAttributed[A](attributes: GeoradiusAttrs, unattributed: ReplyDecoder[A]): ReplyDecoder[attributes.Attributed[A]] =
    if (attributes.isEmpty)
      unattributed.andThen(attributes.decode(ArrayMsg.Empty, attributes.flags, _))
    else {
      case arr@ArrayMsg(IndexedSeq(mem: ValidRedisMsg, _*)) if unattributed.isDefinedAt(mem) =>
        attributes.decode(arr, attributes.flags, unattributed(mem))
    }
}
