package com.avsystem.commons
package redis.commands

import akka.util.ByteString
import com.avsystem.commons.collection.CollectionAliases._
import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion, Opt}
import com.avsystem.commons.redis.CommandEncoder.CommandArg
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.exception.UnexpectedReplyException
import com.avsystem.commons.redis.protocol.{ArrayMsg, BulkStringMsg, IntegerMsg, SimpleStringMsg, ValidRedisMsg}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

trait ClusteredServerApi extends ApiSubset {
  def debugObject(key: Key): Result[ValidRedisMsg] =
    execute(new DebugObject(key))

  private final class DebugObject(key: Key) extends RedisRawCommand with NodeCommand {
    val encoded = encoder("DEBUG", "OBJECT").key(key).result
  }
}

trait NodeServerApi extends ClusteredServerApi {
  def bgrewriteaof: Result[String] =
    execute(Bgrewriteaof)
  def bgsave: Result[String] =
    execute(Bgsave)
  def clientKill(addr: ClientAddress): Result[Unit] =
    execute(new ClientKill(addr))
  def clientKill(filters: ClientFilter*): Result[Long] =
    execute(new ClientKillFiltered(filters))
  def clientList: Result[Seq[ClientInfo]] =
    execute(ClientList)
  def clientPause(timeout: Long): Result[Unit] =
    execute(new ClientPause(timeout))
  def command: Result[Seq[CommandInfo]] =
    execute(Command)
  def commandCount: Result[Long] =
    execute(CommandCount)
  def commandGetkeys(command: RawCommand): Result[Seq[Key]] =
    execute(new CommandGetkeys(command.encoded.elements.iterator.map(_.string)))
  def commandGetkeys(command: Seq[ByteString]): Result[Seq[Key]] =
    execute(new CommandGetkeys(command))
  def commandInfo(commandNames: String*): Result[Seq[CommandInfo]] =
    execute(new CommandInfoCommand(commandNames))
  def configGet(parameter: String): Result[Seq[(String, String)]] =
    execute(new ConfigGet(parameter))
  def configResetstat: Result[Unit] =
    execute(ConfigResetstat)
  def configRewrite: Result[Unit] =
    execute(ConfigRewrite)
  def configSet(parameter: String, value: String): Result[Unit] =
    execute(new ConfigSet(parameter, value))
  def dbsize: Result[Long] =
    execute(Dbsize)
  def debugSegfault: Result[Nothing] =
    execute(DebugSegfault)
  def flushall: Result[Unit] =
    execute(Flushall)
  def flushdb: Result[Unit] =
    execute(Flushdb)
  def info: Result[DefaultRedisInfo] =
    execute(new Info(DefaultRedisInfo, implicitDefault = true))
  def info[T >: FullRedisInfo <: RedisInfo](section: RedisInfoSection[T]): Result[T] =
    execute(new Info(section, implicitDefault = false))
  def lastsave: Result[Long] =
    execute(Lastsave)
  def role: Result[RedisRole] =
    execute(Role)
  def save: Result[Unit] =
    execute(Save)
  def shutdown: Result[Nothing] =
    execute(new Shutdown(Opt.Empty))
  def shutdown(modifier: ShutdownModifier): Result[Nothing] =
    execute(new Shutdown(modifier.opt))
  def slaveof(newMaster: Opt[NodeAddress]): Result[Unit] =
    execute(new Slaveof(newMaster))
  def slowlogGet: Result[Seq[SlowlogEntry]] =
    execute(new SlowlogGet(Opt.Empty))
  def slowlogGet(count: Int): Result[Seq[SlowlogEntry]] =
    execute(new SlowlogGet(count.opt))
  def slowlogLen: Result[Long] =
    execute(SlowlogLen)
  def slowlogReset: Result[Unit] =
    execute(SlowlogReset)
  def time: Result[RedisTimestamp] =
    execute(Time)

  private object Bgrewriteaof extends RedisSimpleStringCommand with NodeCommand {
    val encoded = encoder("BGREWRITEAOF").result
  }

  private object Bgsave extends RedisSimpleStringCommand with NodeCommand {
    val encoded = encoder("BGSAVE").result
  }

  private final class ClientKill(address: ClientAddress) extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("CLIENT", "KILL").add(address.toString).result
  }

  private final class ClientKillFiltered(filters: Seq[ClientFilter]) extends RedisLongCommand with NodeCommand {
    val encoded = {
      val enc = encoder("CLIENT", "KILL")
      if (filters.nonEmpty) {
        enc.add(filters)
      } else {
        // SKIPME is yes by default but is added solely to avoid syntax error when there are no filters
        enc.add("SKIPME").add("yes")
      }
      enc.result
    }
  }

  private object ClientList extends RedisCommand[Seq[ClientInfo]] with NodeCommand {
    val encoded = encoder("CLIENT", "LIST").result
    protected def decodeExpected = {
      case BulkStringMsg(data) =>
        Source.fromInputStream(data.iterator.asInputStream).getLines()
          .map(_.trim).filter(_.nonEmpty).map(line => ClientInfo(line)).to[ArrayBuffer]
    }
  }

  private final class ClientPause(timeout: Long) extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("CLIENT", "PAUSE").add(timeout).result
  }

  private abstract class AbstractCommandInfoCommand extends RedisSeqCommand[CommandInfo] with NodeCommand {
    protected def decodeElement = {
      case ArrayMsg(IndexedSeq(BulkStringMsg(name), IntegerMsg(arity), ArrayMsg(flagArray), IntegerMsg(firstKey), IntegerMsg(lastKey), IntegerMsg(stepCount))) =>
        val flags = flagArray.iterator.map({
          case SimpleStringMsg(flagStr) => CommandFlags.byRepr(flagStr.utf8String)
          case msg => throw new UnexpectedReplyException(s"Expected only simple strings in command flag list, got $msg")
        }).fold(CommandFlags.NoFlags)(_ | _)
        CommandInfo(name.utf8String, CommandArity(math.abs(arity.toInt), arity < 0), flags, firstKey.toInt, lastKey.toInt, stepCount.toInt)
    }
  }

  private object Command extends AbstractCommandInfoCommand {
    val encoded = encoder("COMMAND").result
  }

  private object CommandCount extends RedisLongCommand with NodeCommand {
    val encoded = encoder("COMMAND", "COUNT").result
  }

  private final class CommandGetkeys(command: TraversableOnce[ByteString]) extends RedisDataSeqCommand[Key] with HasKeyCodec with NodeCommand {
    val encoded = encoder("COMMAND", "GETKEYS").add(command).result
  }

  private final class CommandInfoCommand(commandNames: Seq[String]) extends AbstractCommandInfoCommand {
    val encoded = encoder("COMMAND", "INFO").add(commandNames).result
  }

  private final class ConfigGet(parameter: String) extends RedisPairSeqCommand[(String, String)] with NodeCommand {
    val encoded = encoder("CONFIG", "GET").add(parameter).result
    protected def decodeElement = {
      case (BulkStringMsg(param), BulkStringMsg(value)) => (param.utf8String, value.utf8String)
    }
  }

  private object ConfigResetstat extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("CONFIG", "RESETSTAT").result
  }

  private object ConfigRewrite extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("CONFIG", "REWRITE").result
  }

  private final class ConfigSet(parameter: String, value: String) extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("CONFIG", "SET").add(parameter).add(value).result
  }

  private object Dbsize extends RedisLongCommand with NodeCommand {
    val encoded = encoder("DBSIZE").result
  }

  private object DebugSegfault extends RedisCommand[Nothing] with NodeCommand {
    val encoded = encoder("DEBUG", "SEGFAULT").result
    protected def decodeExpected = {
      case msg => throw new UnexpectedReplyException(s"Expected no reply, got $msg")
    }
  }

  private object Flushall extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("FLUSHALL").result
  }

  private object Flushdb extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("FLUSHDB").result
  }

  private final class Info[T >: FullRedisInfo <: RedisInfo](section: RedisInfoSection[T], implicitDefault: Boolean)
    extends RedisCommand[T] with NodeCommand {

    val encoded = encoder("INFO").setup(e => if (!implicitDefault || section != DefaultRedisInfo) e.add(section.name)).result
    protected def decodeExpected = {
      case BulkStringMsg(data) => new FullRedisInfo(data.utf8String)
    }
  }

  private object Lastsave extends RedisLongCommand with NodeCommand {
    val encoded = encoder("LASTSAVE").result
  }

  private object Role extends RedisCommand[RedisRole] with NodeCommand {
    val encoded = encoder("ROLE").result
    protected def decodeExpected = {
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
  }

  private object Save extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("SAVE").result
  }

  private final class Shutdown(modifier: Opt[ShutdownModifier]) extends RedisCommand[Nothing] with NodeCommand {
    val encoded = encoder("SHUTDOWN").add(modifier).result
    protected def decodeExpected = {
      case msg => throw new UnexpectedReplyException(s"Expected no reply, got $msg")
    }
  }

  private final class Slaveof(newMaster: Opt[NodeAddress]) extends RedisUnitCommand with NodeCommand {
    val encoded = {
      val enc = encoder("SLAVEOF")
      newMaster match {
        case Opt.Empty => enc.add("NO").add("ONE")
        case Opt(NodeAddress(ip, port)) => enc.add(ip).add(port)
      }
      enc.result
    }
  }

  private final class SlowlogGet(count: Opt[Int]) extends RedisSeqCommand[SlowlogEntry] with NodeCommand {
    val encoded = encoder("SLOWLOG", "GET").add(count).result
    protected def decodeElement = {
      case ArrayMsg(IndexedSeq(IntegerMsg(id), IntegerMsg(timestamp), IntegerMsg(duration), ArrayMsg(rawCommand))) =>
        val commandArgs = rawCommand.map {
          case BulkStringMsg(arg) => arg
          case el => throw new UnexpectedReplyException(s"Unexpected message for SLOWLOG command argument: $el")
        }
        SlowlogEntry(id, timestamp, duration, commandArgs)
    }
  }

  private object SlowlogLen extends RedisLongCommand with NodeCommand {
    val encoded = encoder("SLOWLOG", "LEN").result
  }

  private object SlowlogReset extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("SLOWLOG", "RESET").result
  }

  private object Time extends RedisCommand[RedisTimestamp] with NodeCommand {
    val encoded = encoder("TIME").result
    protected def decodeExpected = {
      case ArrayMsg(IndexedSeq(BulkStringMsg(seconds), BulkStringMsg(useconds))) =>
        RedisTimestamp(seconds.utf8String.toLong, useconds.utf8String.toLong)
    }
  }
}

trait ConnectionServerApi extends NodeServerApi {
  def clientGetname: Result[Opt[String]] =
    execute(ClientGetname)
  def clientSetname(connectionName: String): Result[Unit] =
    execute(new ClientSetname(connectionName))

  private object ClientGetname extends RedisOptStringCommand with ConnectionCommand {
    val encoded = encoder("CLIENT", "GETNAME").result
  }

  private final class ClientSetname(connectionName: String) extends RedisUnitCommand with ConnectionCommand {
    val encoded = encoder("CLIENT", "SETNAME").add(connectionName).result
  }
}

sealed trait ClientFilter extends Any
case class ClientId(raw: Long) extends AnyVal with ClientFilter {
  override def toString = java.lang.Long.toUnsignedString(raw)
}
object ClientId {
  def apply(str: String): ClientId =
    ClientId(java.lang.Long.parseUnsignedLong(str))
}
final case class ClientAddress(ip: String, port: Int) extends ClientFilter {
  override def toString = s"$ip:$port"
}
object ClientAddress {
  def apply(str: String): ClientAddress = {
    val Array(ip, port) = str.split(':')
    ClientAddress(ip, port.toInt)
  }
}
sealed abstract class ClientType(val name: String) extends NamedEnum with ClientFilter
object ClientType extends NamedEnumCompanion[ClientType] {
  case object Normal extends ClientType("normal")
  case object Master extends ClientType("master")
  case object Slave extends ClientType("slave")
  case object Pubsub extends ClientType("pubsub")

  val values: List[ClientType] = caseObjects
}
case class Skipme(skipme: Boolean) extends AnyVal with ClientFilter

object ClientFilter {
  implicit val commandArg: CommandArg[ClientFilter] = CommandArg {
    case (ce, id: ClientId) => ce.add("ID").add(id.toString)
    case (ce, addr: ClientAddress) => ce.add("ADDR").add(addr.toString)
    case (ce, ct: ClientType) => ce.add("TYPE").add(ct.name)
    case (ce, Skipme(value)) => ce.add("SKIPME").add(if (value) "yes" else "no")
  }
}

class ClientFlags(val raw: Int) extends AnyVal {

  import ClientFlags._

  def |(other: ClientFlags) = new ClientFlags(raw | other.raw)
  def &(other: ClientFlags) = new ClientFlags(raw & other.raw)
  def ^(other: ClientFlags) = new ClientFlags(raw ^ other.raw)
  def unary_~ : ClientFlags = new ClientFlags(~raw)

  def slaveMonitor = (this & O) != NoFlags
  def slave = (this & S) != NoFlags
  def master = (this & M) != NoFlags
  def transaction = (this & x) != NoFlags
  def waitingBlocking = (this & b) != NoFlags
  def waitingVMIO = (this & i) != NoFlags
  def dirty = (this & d) != NoFlags
  def closingAfterReply = (this & c) != NoFlags
  def unblocked = (this & u) != NoFlags
  def unixSocket = (this & U) != NoFlags
  def clusterReadonly = (this & r) != NoFlags
  def closingASAP = (this & A) != NoFlags

  override def toString =
    if (this == NoFlags) "N"
    else reprValuePairs.iterator.collect {
      case (ch, f) if (this & f) != NoFlags => ch
    }.mkString
}
object ClientFlags {
  val NoFlags = new ClientFlags(0)
  val O = new ClientFlags(1 << 0)
  val S = new ClientFlags(1 << 1)
  val M = new ClientFlags(1 << 2)
  val x = new ClientFlags(1 << 3)
  val b = new ClientFlags(1 << 4)
  val i = new ClientFlags(1 << 5)
  val d = new ClientFlags(1 << 6)
  val c = new ClientFlags(1 << 7)
  val u = new ClientFlags(1 << 8)
  val U = new ClientFlags(1 << 9)
  val r = new ClientFlags(1 << 10)
  val A = new ClientFlags(1 << 11)

  private val reprValuePairs = Seq(
    'O' -> O,
    'S' -> S,
    'M' -> M,
    'x' -> x,
    'b' -> b,
    'i' -> i,
    'd' -> d,
    'c' -> c,
    'u' -> u,
    'U' -> U,
    'r' -> r,
    'A' -> A
  )

  def apply(str: String): ClientFlags =
    reprValuePairs.foldLeft(NoFlags) {
      case (acc, (ch, ev)) => if (str.contains(ch)) acc | ev else acc
    }
}

class ClientEvents(val raw: Int) extends AnyVal {

  import ClientEvents._

  def |(other: ClientEvents) = new ClientEvents(raw | other.raw)
  def &(other: ClientEvents) = new ClientEvents(raw & other.raw)
  def ^(other: ClientEvents) = new ClientEvents(raw ^ other.raw)
  def unary_~ : ClientEvents = new ClientEvents(~raw)

  def readable = (this & r) != NoEvents
  def writable = (this & w) != NoEvents

  override def toString =
    reprValuePairs.iterator.collect {
      case (ch, ev) if (this & ev) != NoEvents => ch
    }.mkString
}
object ClientEvents {
  val NoEvents = new ClientEvents(0)
  val r = new ClientEvents(1 << 0)
  val w = new ClientEvents(1 << 1)

  private val reprValuePairs = Seq(
    'r' -> r,
    'w' -> w
  )

  def apply(str: String): ClientEvents =
    reprValuePairs.foldLeft(NoEvents) {
      case (acc, (ch, ev)) => if (str.contains(ch)) acc | ev else acc
    }
}

case class ClientInfo(infoLine: String) {
  private val attrMap =
    MHashMap() ++ infoLine.split(" ").iterator.map { attr =>
      val Array(name, value) = attr.split("=", 2)
      (name, value)
    }

  def id = ClientId(attrMap("id"))
  def addr = ClientAddress(attrMap("addr"))
  def fd = attrMap("fd").toInt
  def name = attrMap("name")
  def age = attrMap("age").toLong
  def idle = attrMap("idle").toLong
  def flags = ClientFlags(attrMap("flags"))
  def db = attrMap("db").toInt
  def sub = attrMap("sub").toInt
  def psub = attrMap("psub").toInt
  def multi = attrMap("multi").toInt
  def qbuf = attrMap("qbuf").toLong
  def qbufFree = attrMap("qbuf-free").toLong
  def obl = attrMap("obl").toLong
  def oll = attrMap("oll").toLong
  def omem = attrMap("omem").toLong
  def events = ClientEvents(attrMap("events"))
  def cmd = attrMap("cmd").opt.filter(_ != "NULL")

  override def toString = infoLine
}

case class CommandInfo(
  name: String,
  arity: CommandArity,
  flags: CommandFlags,
  firstKey: Int,
  lastKey: Int,
  stepCount: Int
)
case class CommandArity(arity: Int, more: Boolean)

class CommandFlags(val raw: Int) extends AnyVal {

  import CommandFlags._

  def |(other: CommandFlags) = new CommandFlags(raw | other.raw)
  def &(other: CommandFlags) = new CommandFlags(raw & other.raw)
  def ^(other: CommandFlags) = new CommandFlags(raw ^ other.raw)
  def unary_~ : CommandFlags = new CommandFlags(~raw)

  def write = (this & Write) != NoFlags
  def readonly = (this & Readonly) != NoFlags
  def denyoom = (this & Denyoom) != NoFlags
  def admin = (this & Admin) != NoFlags
  def pubsub = (this & Pubsub) != NoFlags
  def noscript = (this & Noscript) != NoFlags
  def random = (this & Random) != NoFlags
  def sortForScript = (this & SortForScript) != NoFlags
  def loading = (this & Loading) != NoFlags
  def stale = (this & Stale) != NoFlags
  def skipMonitor = (this & SkipMonitor) != NoFlags
  def asking = (this & Asking) != NoFlags
  def fast = (this & Fast) != NoFlags
  def movablekeys = (this & Movablekeys) != NoFlags

  override def toString =
    byRepr.iterator.collect({ case (repr, flag) if (this & flag) != NoFlags => repr }).mkString("CommandFlags(", ",", ")")
}
object CommandFlags {
  val NoFlags = new CommandFlags(0)
  val Write = new CommandFlags(1 << 0)
  val Readonly = new CommandFlags(1 << 1)
  val Denyoom = new CommandFlags(1 << 2)
  val Admin = new CommandFlags(1 << 3)
  val Pubsub = new CommandFlags(1 << 4)
  val Noscript = new CommandFlags(1 << 5)
  val Random = new CommandFlags(1 << 6)
  val SortForScript = new CommandFlags(1 << 7)
  val Loading = new CommandFlags(1 << 8)
  val Stale = new CommandFlags(1 << 9)
  val SkipMonitor = new CommandFlags(1 << 10)
  val Asking = new CommandFlags(1 << 11)
  val Fast = new CommandFlags(1 << 12)
  val Movablekeys = new CommandFlags(1 << 13)

  val byRepr = Map(
    "write" -> Write,
    "readonly" -> Readonly,
    "denyoom" -> Denyoom,
    "admin" -> Admin,
    "pubsub" -> Pubsub,
    "noscript" -> Noscript,
    "random" -> Random,
    "sort_for_script" -> SortForScript,
    "loading" -> Loading,
    "stale" -> Stale,
    "skip_monitor" -> SkipMonitor,
    "asking" -> Asking,
    "fast" -> Fast,
    "movablekeys" -> Movablekeys
  )
}

sealed trait RedisRole
case class MasterRole(replOffset: Long, slaveOffsets: Seq[(NodeAddress, Long)]) extends RedisRole
case class SlaveRole(master: NodeAddress, replState: ReplState, receivedDataOffset: Long) extends RedisRole
case class SentinelRole(masterNames: Seq[String]) extends RedisRole
object RedisRole {
  val MasterStr = BulkStringMsg(ByteString("master"))
  val SlaveStr = BulkStringMsg(ByteString("slave"))
  val SentinelStr = BulkStringMsg(ByteString("sentinel"))
}

sealed abstract class ReplState(val name: String) extends NamedEnum
object ReplState extends NamedEnumCompanion[ReplState] {
  case object Connect extends ReplState("connect")
  case object Connecting extends ReplState("connecting")
  case object Connected extends ReplState("connected")
  val values: List[ReplState] = caseObjects
}

sealed abstract class ShutdownModifier(val name: String) extends NamedEnum
object ShutdownModifier extends NamedEnumCompanion[ShutdownModifier] {
  case object Save extends ReplState("SAVE")
  case object Nosave extends ReplState("NOSAVE")
  val values: List[ShutdownModifier] = caseObjects
}

case class SlowlogEntry(id: Long, timestamp: Long, duration: Long, command: Seq[ByteString])

case class RedisTimestamp(seconds: Long, useconds: Long)
