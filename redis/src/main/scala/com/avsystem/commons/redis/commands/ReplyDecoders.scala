package com.avsystem.commons
package redis.commands

import org.apache.pekko.util.ByteString
import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion}
import com.avsystem.commons.redis.exception.UnexpectedReplyException
import com.avsystem.commons.redis.protocol._
import com.avsystem.commons.redis.util.SizedArraySeqFactory
import com.avsystem.commons.redis.{NodeAddress, RedisDataCodec, RedisRecordCodec}

import scala.collection.mutable
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

  val simpleOkAsUnit: ReplyDecoder[Unit] = {
    case RedisMsg.Ok => ()
  }

  val nullBulkOrSimpleOkAsBoolean: ReplyDecoder[Boolean] = {
    case RedisMsg.Ok => true
    case NullBulkStringMsg => false
  }

  val integerAsLong: ReplyDecoder[Long] = {
    case IntegerMsg(value) => value
  }

  val positiveIntegerAsLongOpt: ReplyDecoder[Opt[Long]] = {
    case IntegerMsg(value) => if (value > 0) value.opt else Opt.Empty
  }

  val integerAsInt: ReplyDecoder[Int] = {
    case IntegerMsg(value) => value.toInt
  }

  val integerAsBoolean: ReplyDecoder[Boolean] = {
    case IntegerMsg(0) => false
    case IntegerMsg(1) => true
  }

  val integerAsTtl: ReplyDecoder[Opt[Opt[Long]]] = {
    case IntegerMsg(-2) => Opt.Empty
    case IntegerMsg(-1) => Opt(Opt.Empty)
    case IntegerMsg(ttl) => Opt(Opt(ttl))
  }

  val integerAsClientId: ReplyDecoder[ClientId] = {
    case IntegerMsg(value) => ClientId(value)
  }

  def bulkAsNamedEnum[E <: NamedEnum](companion: NamedEnumCompanion[E]): ReplyDecoder[E] = {
    case BulkStringMsg(data) => companion.byName(data.utf8String)
  }

  val bulkAsLong: ReplyDecoder[Long] =
    bulk(_.utf8String.toLong)

  val bulkAsInt: ReplyDecoder[Long] =
    bulk(_.utf8String.toInt)

  val bulkAsDouble: ReplyDecoder[Double] =
    bulk(_.utf8String.toDouble)

  val bulkAsUTF8: ReplyDecoder[String] =
    bulk(_.utf8String)

  val bulkAsBinary: ReplyDecoder[ByteString] =
    bulk(bs => bs)

  val bulkAsSha1: ReplyDecoder[Sha1] =
    bulk(bs => Sha1(bs.utf8String))

  val bulkAsNodeId: ReplyDecoder[NodeId] =
    bulk(bs => NodeId(bs.utf8String))

  val bulkAsClientInfos: ReplyDecoder[Seq[ClientInfo]] = {
    case BulkStringMsg(data) =>
      Source.fromInputStream(data.iterator.asInputStream).getLines()
        .map(_.trim).filter(_.nonEmpty).map(line => ClientInfo(line)).toIndexedSeq
  }

  val bulkAsNodeInfos: ReplyDecoder[Seq[NodeInfo]] = {
    case BulkStringMsg(nodeInfos) =>
      nodeInfos.utf8String.split("\n").iterator.filter(_.nonEmpty).map(NodeInfo).toIndexedSeq
  }

  val bulkAsNodeInfo: ReplyDecoder[NodeInfo] =
    bulk(bs => NodeInfo(bs.utf8String))

  val multiBulkAsNodeInfos: ReplyDecoder[Seq[NodeInfo]] =
    multiBulkAsSeq(bulkAsNodeInfo)

  val bulkAsCursor: ReplyDecoder[Cursor] = {
    case BulkStringMsg(data) => Cursor(data.utf8String.toLong)
  }

  val bulkAsXEntryId: ReplyDecoder[XEntryId] = {
    case BulkStringMsg(data) => XEntryId.parse(data.utf8String)
  }

  val bulkAsXGroup: ReplyDecoder[XGroup] = {
    case BulkStringMsg(str) => XGroup(str.utf8String)
  }

  val bulkAsXConsumer: ReplyDecoder[XConsumer] = {
    case BulkStringMsg(str) => XConsumer(str.utf8String)
  }

  val simpleAsUTF8: ReplyDecoder[String] =
    simple(_.utf8String)

  val simpleAsBinary: ReplyDecoder[ByteString] =
    simple(bs => bs)

  val simpleBumpepochResult: ReplyDecoder[BumpepochResult] = {
    case BumpepochResult.Bumped.encoded => BumpepochResult.Bumped
    case BumpepochResult.Still.encoded => BumpepochResult.Still
  }

  def simple[T](fun: ByteString => T): ReplyDecoder[T] = {
    case SimpleStringMsg(data) => fun(data)
  }

  def simpleAs[T: RedisDataCodec]: ReplyDecoder[T] =
    simple(RedisDataCodec[T].read)

  def bulk[T](fun: ByteString => T): ReplyDecoder[T] = {
    case BulkStringMsg(data) => fun(data)
  }

  def bulkAs[T: RedisDataCodec]: ReplyDecoder[T] =
    bulk(RedisDataCodec[T].read)

  def nullBulkOr[T](decoder: ReplyDecoder[T]): ReplyDecoder[Opt[T]] =
    decoder.andThen(_.opt) unless {
      case NullBulkStringMsg => Opt.Empty
    }

  def nullBulkOrAs[T: RedisDataCodec]: ReplyDecoder[Opt[T]] =
    nullBulkOr(bulkAs[T])

  def nullOrEmptyBulkOr[T](decoder: ReplyDecoder[T]): ReplyDecoder[Opt[T]] =
    decoder.andThen(_.opt) unless {
      case NullBulkStringMsg | BulkStringMsg(ByteString.empty) => Opt.Empty
    }

  def nullOrEmptyBulkOr[T](fun: ByteString => T): ReplyDecoder[Opt[T]] =
    nullOrEmptyBulkOr(bulk(fun))

  def nullOrEmptyBulkOrAs[T: RedisDataCodec]: ReplyDecoder[Opt[T]] =
    nullOrEmptyBulkOr(RedisDataCodec[T].read)

  private def multiBulkIterator[T](elements: Seq[RedisMsg], elementDecoder: ReplyDecoder[T]): Iterator[T] =
    elements.iterator.map {
      case vrm: ValidRedisMsg => elementDecoder.applyOrElse(vrm, (_: ValidRedisMsg) =>
        throw new UnexpectedReplyException(s"Unexpected element in multi-bulk reply: $vrm"))
      case msg => throw new UnexpectedReplyException(msg.toString)
    }

  def multiBulkAsSeq[T](elementDecoder: ReplyDecoder[T]): ReplyDecoder[Seq[T]] = {
    case ArrayMsg(elements) => multiBulkIterator(elements, elementDecoder).to(new SizedArraySeqFactory[T](elements.size))
  }

  def multiBulkAsSeqOf[T: RedisDataCodec]: ReplyDecoder[Seq[T]] =
    multiBulkAsSeq(bulkAs[T])

  def multiBulkAsSet[T](elementDecoder: ReplyDecoder[T]): ReplyDecoder[BSet[T]] = {
    case ArrayMsg(elements) => multiBulkIterator(elements, elementDecoder).toSized(MHashSet, elements.size)
  }

  def multiBulkAsSetOf[T: RedisDataCodec]: ReplyDecoder[BSet[T]] =
    multiBulkAsSet(bulkAs[T])

  def multiBulkAsPair[A, B](firstDecoder: ReplyDecoder[A], secondDecoder: ReplyDecoder[B]): ReplyDecoder[(A, B)] = {
    case ArrayMsg(IndexedSeq(f: ValidRedisMsg, s: ValidRedisMsg)) =>
      val first = firstDecoder.applyOrElse(f, (_: ValidRedisMsg) =>
        throw new UnexpectedReplyException(s"Unexpected first element in multi-bulk reply: $f"))
      val second = secondDecoder.applyOrElse(s, (_: ValidRedisMsg) =>
        throw new UnexpectedReplyException(s"Unexpected second element in multi-bulk reply: $s"))
      (first, second)
  }

  def multiBulkAsMap[A, B](keyDecoder: ReplyDecoder[A], valueDecoder: ReplyDecoder[B]): ReplyDecoder[BMap[A, B]] = {
    case ArrayMsg(elements) => new mutable.HashMap[A, B]() ++
      multiBulkIterator(elements, multiBulkAsPair(keyDecoder, valueDecoder))
  }

  def multiBulkAsMapOf[A: RedisDataCodec, B: RedisDataCodec]: ReplyDecoder[BMap[A, B]] =
    multiBulkAsMap(bulkAs[A], bulkAs[B])

  def multiBulkAsZTripleOf[K: RedisDataCodec, V: RedisDataCodec]: ReplyDecoder[Opt[(K, V, Double)]] = {
    case NullArrayMsg => Opt.Empty
    case ArrayMsg(IndexedSeq(BulkStringMsg(key), BulkStringMsg(value), BulkStringMsg(score))) =>
      Opt(RedisDataCodec.read[K](key), RedisDataCodec.read[V](value), score.utf8String.toDouble)
  }

  val multiBulkAsGeoPoint: ReplyDecoder[GeoPoint] = {
    case ArrayMsg(IndexedSeq(BulkStringMsg(rawLong), BulkStringMsg(rawLat))) =>
      GeoPoint(rawLong.utf8String.toDouble, rawLat.utf8String.toDouble)
  }

  val multiBulkAsCommandInfo: ReplyDecoder[CommandInfo] = {
    case ArrayMsg(IndexedSeq(BulkStringMsg(name), IntegerMsg(arity), ArrayMsg(flagArray), IntegerMsg(firstKey), IntegerMsg(lastKey), IntegerMsg(stepCount), _*)) =>
      val flags = flagArray.iterator.map({
        case SimpleStringMsg(flagStr) => CommandFlags.byRepr.getOrElse(flagStr.utf8String, CommandFlags.NoFlags)
        case msg => throw new UnexpectedReplyException(s"Expected only simple strings in command flag list, got $msg")
      }).fold(CommandFlags.NoFlags)(_ | _)
      CommandInfo(
        name.utf8String, CommandArity(math.abs(arity.toInt), arity < 0),
        flags, firstKey.toInt, lastKey.toInt, stepCount.toInt
      )
  }

  val multiBulkAsRedisRole: ReplyDecoder[RedisRole] = {
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

  val multiBulkAsSlowlogEntry: ReplyDecoder[SlowlogEntry] = {
    case msg@ArrayMsg(IndexedSeq(IntegerMsg(id), IntegerMsg(timestamp), IntegerMsg(duration), ArrayMsg(rawCommand), rest@_*)) =>
      val commandArgs = rawCommand.map {
        case BulkStringMsg(arg) => arg
        case el => throw new UnexpectedReplyException(s"Unexpected message for SLOWLOG command argument: $el")
      }
      val (clientAddr, clientName) = rest match {
        case IndexedSeq(BulkStringMsg(addr)) => (ClientAddress(addr.utf8String).opt, Opt.Empty)
        case IndexedSeq(BulkStringMsg(addr), NullBulkStringMsg, _*) => (ClientAddress(addr.utf8String).opt, Opt.Empty)
        case IndexedSeq(BulkStringMsg(addr), BulkStringMsg(name), _*) => (ClientAddress(addr.utf8String).opt, name.utf8String.opt)
        case _ => throw new UnexpectedReplyException(s"Unexpected message for SLOWLOG command argument: $msg")
      }
      SlowlogEntry(id, timestamp, duration, commandArgs, clientAddr, clientName)
  }

  val multiBulkAsRedisTimestamp: ReplyDecoder[RedisTimestamp] = {
    case ArrayMsg(IndexedSeq(BulkStringMsg(seconds), BulkStringMsg(useconds))) =>
      RedisTimestamp(seconds.utf8String.toLong, useconds.utf8String.toLong)
  }

  val multiBulkAsSlotRangeMapping: ReplyDecoder[SlotRangeMapping] = {
    case ArrayMsg(IndexedSeq(IntegerMsg(from), IntegerMsg(to), master, slaves@_*)) =>
      val range = SlotRange(from.toInt, to.toInt)
      def parseNode(rr: RedisMsg) = rr match {
        case ArrayMsg(IndexedSeq(BulkStringMsg(ip), IntegerMsg(port), BulkStringMsg(nodeId), _*)) =>
          (NodeAddress(ip.utf8String, port.toInt), NodeId(nodeId.utf8String).opt)
        case ArrayMsg(IndexedSeq(BulkStringMsg(ip), IntegerMsg(port))) =>
          (NodeAddress(ip.utf8String, port.toInt), Opt.Empty)
        case _ =>
          throw new UnexpectedReplyException(s"bad entry in CLUSTER SLOTS reply: $rr")
      }
      val (masterAddr, masterId) = parseNode(master)
      SlotRangeMapping(range, masterAddr, masterId, slaves.map(parseNode))
  }

  val multiBulkAsXPendingOverview: ReplyDecoder[XPendingOverview] = {
    case ArrayMsg(IndexedSeq(IntegerMsg(0), NullBulkStringMsg, NullBulkStringMsg, NullArrayMsg)) =>
      XPendingOverview.Empty
    case ArrayMsg(IndexedSeq(IntegerMsg(count), BulkStringMsg(minid), BulkStringMsg(maxid), ArrayMsg(byConsumer))) =>
      XPendingOverview(
        count, XEntryId.parse(minid.utf8String), XEntryId.parse(maxid.utf8String),
        new mutable.HashMap() ++ multiBulkIterator(byConsumer, multiBulkAsPair(bulkAsXConsumer, bulkAsLong))
      )
  }

  val multiBulkAsXPendingEntry: ReplyDecoder[XPendingEntry] = {
    case ArrayMsg(IndexedSeq(BulkStringMsg(id), BulkStringMsg(consumer), IntegerMsg(idle), IntegerMsg(delivered))) =>
      XPendingEntry(XEntryId.parse(id.utf8String), XConsumer(consumer.utf8String), idle, delivered.toInt)
  }

  def multiBulkAsXEntryOf[R: RedisRecordCodec]: ReplyDecoder[XEntry[R]] = {
    case ArrayMsg(IndexedSeq(BulkStringMsg(id), data: ArrayMsg[RedisMsg])) =>
      XEntry(XEntryId.parse(id.utf8String), flatMultiBulkAsRecord[R].apply(data))
  }

  def multiBulkAsXEntriesMapOf[K: RedisDataCodec, R: RedisRecordCodec]: ReplyDecoder[BMap[K, Seq[XEntry[R]]]] =
    multiBulkAsMap(bulkAs[K], multiBulkAsSeq(multiBulkAsXEntryOf[R])) unless {
      case NullArrayMsg => Map.empty
    }

  val multiBulkAsXConsumerInfo: ReplyDecoder[XConsumerInfo] =
    flatMultiBulkAsMap(bulkAsUTF8, undecoded).andThen(XConsumerInfo)

  val multiBulkAsXGroupInfo: ReplyDecoder[XGroupInfo] =
    flatMultiBulkAsMap(bulkAsUTF8, undecoded).andThen(XGroupInfo)

  def multiBulkAsXStreamInfoOf[Rec: RedisRecordCodec]: ReplyDecoder[XStreamInfo[Rec]] =
    flatMultiBulkAsMap(bulkAsUTF8, undecoded).andThen(XStreamInfo[Rec](_))

  def multiBulkAsGroupedSeq[T](size: Int, elementDecoder: ReplyDecoder[T]): ReplyDecoder[Seq[Seq[T]]] = {
    case ArrayMsg(elements) =>
      def elemDecode(msg: RedisMsg): T = msg match {
        case vrm: ValidRedisMsg => elementDecoder.applyOrElse(vrm, (_: ValidRedisMsg) =>
          throw new UnexpectedReplyException(vrm.toString))
        case _ => throw new UnexpectedReplyException(msg.toString)
      }
      elements.iterator.grouped(size)
        .map(_.iterator.map(elemDecode).to(new SizedArraySeqFactory[T](size)))
        .to(new SizedArraySeqFactory(elements.size / size))
  }

  def nullMultiBulkOr[T](decoder: ReplyDecoder[T]): ReplyDecoder[Opt[T]] =
    decoder.andThen(_.opt) unless {
      case NullArrayMsg => Opt.Empty
    }

  def nullMultiBulkOrAs[T: RedisDataCodec]: ReplyDecoder[Opt[T]] =
    nullMultiBulkOr(bulkAs[T])

  def flatPairMultiBulkAsSeq[T](pairDecoder: ReplyPairDecoder[T]): ReplyDecoder[Seq[T]] = {
    case ArrayMsg(elements) => elements.iterator.pairs.map {
      case (first: ValidRedisMsg, second: ValidRedisMsg) => pairDecoder.applyOrElse((first, second),
        (p: (ValidRedisMsg, ValidRedisMsg)) => throw new UnexpectedReplyException(s"Unexpected element pair in multi-bulk reply: $p"))
      case p => throw new UnexpectedReplyException(s"Unexpected element pair in multi-bulk reply: $p")
    }.to(new SizedArraySeqFactory[T](elements.size / 2))
  }

  private def flatPairedMultiBulkIterator[A, B](elements: Seq[RedisMsg], firstDecoder: ReplyDecoder[A], secondDecoder: ReplyDecoder[B]): Iterator[(A, B)] =
    elements.iterator.pairs.map {
      case (f: ValidRedisMsg, s: ValidRedisMsg) =>
        val first = firstDecoder.applyOrElse(f, (_: ValidRedisMsg) =>
          throw new UnexpectedReplyException(s"Unexpected element in multi-bulk reply: $f"))
        val second = secondDecoder.applyOrElse(s, (_: ValidRedisMsg) =>
          throw new UnexpectedReplyException(s"Unexpected element in multi-bulk reply: $s"))
        (first, second)
      case p =>
        throw new UnexpectedReplyException(s"Unexpected element pair in multi-bulk reply: $p")
    }

  def flatMultiBulkAsPairSeq[A, B](firstDecoder: ReplyDecoder[A], secondDecoder: ReplyDecoder[B]): ReplyDecoder[Seq[(A, B)]] = {
    case ArrayMsg(elements) =>
      flatPairedMultiBulkIterator(elements, firstDecoder, secondDecoder).to(new SizedArraySeqFactory(elements.size / 2))
  }

  def flatMultiBulkAsSwappedPairSeq[A, B](firstDecoder: ReplyDecoder[A], secondDecoder: ReplyDecoder[B]): ReplyDecoder[Seq[(B, A)]] = {
    case ArrayMsg(elements) =>
      flatPairedMultiBulkIterator(elements, firstDecoder, secondDecoder).map(_.swap)
        .to(new SizedArraySeqFactory(elements.size / 2))
  }

  def flatMultiBulkAsMap[A, B](keyDecoder: ReplyDecoder[A], valueDecoder: ReplyDecoder[B]): ReplyDecoder[BMap[A, B]] = {
    case ArrayMsg(elements) => new mutable.HashMap() ++
      flatPairedMultiBulkIterator(elements, keyDecoder, valueDecoder)
  }

  def flatMultiBulkAsPairSeqOf[A: RedisDataCodec, B: RedisDataCodec]: ReplyDecoder[Seq[(A, B)]] =
    flatMultiBulkAsPairSeq(bulkAs[A], bulkAs[B])

  def flatMultiBulkAsMapOf[A: RedisDataCodec, B: RedisDataCodec]: ReplyDecoder[BMap[A, B]] =
    flatMultiBulkAsMap(bulkAs[A], bulkAs[B])

  def flatMultiBulkAsRecord[R: RedisRecordCodec]: ReplyDecoder[R] = {
    case ArrayMsg(elements: IndexedSeq[BulkStringMsg@unchecked]) if elements.forall(_.isInstanceOf[BulkStringMsg]) =>
      RedisRecordCodec[R].read(elements)
  }

  def flatMultiBulkAsRecordOpt[R: RedisRecordCodec]: ReplyDecoder[Opt[R]] =
    flatMultiBulkAsRecord[R].andThen(_.opt) unless {
      case ArrayMsg.Empty => Opt.Empty
    }

  def geoAttributed[A](attributes: GeoradiusAttrs, unattributed: ReplyDecoder[A]): ReplyDecoder[attributes.Attributed[A]] =
    if (attributes.isEmpty)
      unattributed.andThen(attributes.decode(ArrayMsg.Empty, attributes.flags, _))
    else {
      case arr@ArrayMsg(IndexedSeq(mem: ValidRedisMsg, _*)) if unattributed.isDefinedAt(mem) =>
        attributes.decode(arr, attributes.flags, unattributed(mem))
    }

  val multiBulkAsNodeAddress: ReplyDecoder[NodeAddress] = {
    case ArrayMsg(IndexedSeq(BulkStringMsg(ip), BulkStringMsg(port))) =>
      NodeAddress(ip.utf8String, port.utf8String.toInt)
  }

  val pubSubEvent: ReplyDecoder[PubSubEvent] = {
    case ArrayMsg(IndexedSeq(PubSubEvent.MessageStr, BulkStringMsg(channel), message: ValidRedisMsg)) =>
      PubSubEvent.Message(channel.utf8String, message)
    case ArrayMsg(IndexedSeq(PubSubEvent.PmessageStr, BulkStringMsg(pattern), BulkStringMsg(channel), message: ValidRedisMsg)) =>
      PubSubEvent.Pmessage(pattern.utf8String, channel.utf8String, message)
    case ArrayMsg(IndexedSeq(PubSubEvent.SubscribeStr, BulkStringMsg(channel), IntegerMsg(subscribed))) =>
      PubSubEvent.Subscribe(channel.utf8String, subscribed.toInt)
    case ArrayMsg(IndexedSeq(PubSubEvent.PsubscribeStr, BulkStringMsg(pattern), IntegerMsg(subscribed))) =>
      PubSubEvent.Psubscribe(pattern.utf8String, subscribed.toInt)
    case ArrayMsg(IndexedSeq(PubSubEvent.UnsubscribeStr, BulkStringMsg(channel), IntegerMsg(subscribed))) =>
      PubSubEvent.Unsubscribe(channel.utf8String, subscribed.toInt)
    case ArrayMsg(IndexedSeq(PubSubEvent.PunsubscribeStr, BulkStringMsg(pattern), IntegerMsg(subscribed))) =>
      PubSubEvent.Punsubscribe(pattern.utf8String, subscribed.toInt)
  }
}
