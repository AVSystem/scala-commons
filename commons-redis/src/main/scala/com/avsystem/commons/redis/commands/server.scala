package com.avsystem.commons
package redis.commands

import akka.util.ByteString
import com.avsystem.commons.collection.CollectionAliases._
import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion, Opt, OptArg}
import com.avsystem.commons.redis.CommandEncoder.CommandArg
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.commands.ReplyDecoders._
import com.avsystem.commons.redis.protocol.BulkStringMsg

trait NodeServerApi extends ApiSubset {
  /** [[http://redis.io/commands/bgrewriteaof BGREWRITEAOF]] */
  def bgrewriteaof: Result[String] =
    execute(Bgrewriteaof)
  /** [[http://redis.io/commands/bgsave BGSAVE]] */
  def bgsave: Result[String] = bgsave()
  /** [[http://redis.io/commands/bgsave BGSAVE]] */
  def bgsave(schedule: Boolean = false): Result[String] =
    execute(new Bgsave(schedule))
  /** [[http://redis.io/commands/client-kill CLIENT KILL]] */
  def clientKill(addr: ClientAddress): Result[Unit] =
    execute(new ClientKill(addr))
  /** [[http://redis.io/commands/client-kill CLIENT KILL]] */
  def clientKill(filters: ClientFilter*): Result[Int] =
    execute(new ClientKillFiltered(filters))
  /** [[http://redis.io/commands/client-list CLIENT LIST]] */
  def clientList: Result[Seq[ClientInfo]] =
    execute(ClientList)
  /** [[http://redis.io/commands/client-pause CLIENT PAUSE]] */
  def clientPause(timeout: Long): Result[Unit] =
    execute(new ClientPause(timeout))
  /** [[http://redis.io/commands/command COMMAND]] */
  def command: Result[Seq[CommandInfo]] =
    execute(Command)
  /** [[http://redis.io/commands/command-count COMMAND COUNT]] */
  def commandCount: Result[Int] =
    execute(CommandCount)
  /** [[http://redis.io/commands/command-getkeys COMMAND GETKEYS]] */
  def commandGetkeys(command: RawCommand): Result[Seq[Key]] =
    execute(new CommandGetkeys(command.encoded.elements.iterator.map(_.string)))
  /** [[http://redis.io/commands/command-getkeys COMMAND GETKEYS]] */
  def commandGetkeys(command: Seq[ByteString]): Result[Seq[Key]] =
    execute(new CommandGetkeys(command))
  /** [[http://redis.io/commands/command-info COMMAND INFO]] */
  def commandInfo(commandName: String): Result[CommandInfo] =
    execute(new CommandInfoCommand(commandName.single).map(_.head))
  /** [[http://redis.io/commands/command-info COMMAND INFO]] */
  def commandInfo(commandName: String, commandNames: String*): Result[Seq[CommandInfo]] =
    execute(new CommandInfoCommand(commandName +:: commandNames))
  /** [[http://redis.io/commands/command-info COMMAND INFO]] */
  def commandInfo(commandNames: Seq[String]): Result[Seq[CommandInfo]] =
    execute(new CommandInfoCommand(commandNames))
  /** [[http://redis.io/commands/config-get CONFIG GET]] */
  def configGet(parameter: String): Result[Seq[(String, String)]] =
    execute(new ConfigGet(parameter))
  /** [[http://redis.io/commands/config-resetstat CONFIG RESETSTAT]] */
  def configResetstat: Result[Unit] =
    execute(ConfigResetstat)
  /** [[http://redis.io/commands/config-rewrite CONFIG REWRITE]] */
  def configRewrite: Result[Unit] =
    execute(ConfigRewrite)
  /** [[http://redis.io/commands/config-set CONFIG SET]] */
  def configSet(parameter: String, value: String): Result[Unit] =
    execute(new ConfigSet(parameter, value))
  /** [[http://redis.io/commands/dbsize DBSIZE]] */
  def dbsize: Result[Long] =
    execute(Dbsize)
  /** [[http://redis.io/commands/debug-segfault DEBUG SEGFAULT]] */
  def debugSegfault: Result[Nothing] =
    execute(DebugSegfault)
  /** [[http://redis.io/commands/flushall FLUSHALL]] */
  def flushall: Result[Unit] =
    execute(Flushall)
  /** [[http://redis.io/commands/flushdb FLUSHDB]] */
  def flushdb: Result[Unit] =
    execute(Flushdb)
  /** [[http://redis.io/commands/info INFO]] */
  def info: Result[DefaultRedisInfo] =
    execute(new Info(DefaultRedisInfo, implicitDefault = true))
  /** [[http://redis.io/commands/info INFO]] */
  def info[T >: FullRedisInfo <: RedisInfo](section: RedisInfoSection[T]): Result[T] =
    execute(new Info(section, implicitDefault = false))
  /** [[http://redis.io/commands/lastsave LASTSAVE]] */
  def lastsave: Result[Long] =
    execute(Lastsave)
  /** [[http://redis.io/commands/role ROLE]] */
  def role: Result[RedisRole] =
    execute(Role)
  /** [[http://redis.io/commands/save SAVE]] */
  def save: Result[Unit] =
    execute(Save)
  /** [[http://redis.io/commands/shutdown SHUTDOWN]] */
  def shutdown: Result[Nothing] = shutdown()
  /** [[http://redis.io/commands/shutdown SHUTDOWN]] */
  def shutdown(modifier: OptArg[ShutdownModifier] = OptArg.Empty): Result[Nothing] =
    execute(new Shutdown(modifier.toOpt))
  /** [[http://redis.io/commands/slaveof SLAVEOF]] */
  def slaveofNoOne: Result[Unit] =
    execute(new Slaveof(Opt.Empty))
  /** [[http://redis.io/commands/slaveof SLAVEOF]] */
  def slaveof(newMaster: NodeAddress): Result[Unit] =
    execute(new Slaveof(newMaster.opt))
  /** [[http://redis.io/commands/slowlog SLOWLOG]] */
  def slowlogGet: Result[Seq[SlowlogEntry]] = slowlogGet()
  /** [[http://redis.io/commands/slowlog SLOWLOG]] */
  def slowlogGet(count: OptArg[Int] = OptArg.Empty): Result[Seq[SlowlogEntry]] =
    execute(new SlowlogGet(count.toOpt))
  /** [[http://redis.io/commands/slowlog SLOWLOG]] */
  def slowlogLen: Result[Long] =
    execute(SlowlogLen)
  /** [[http://redis.io/commands/slowlog SLOWLOG]] */
  def slowlogReset: Result[Unit] =
    execute(SlowlogReset)
  /** [[http://redis.io/commands/time TIME]] */
  def time: Result[RedisTimestamp] =
    execute(Time)

  private object Bgrewriteaof extends RedisSimpleStringCommand with NodeCommand {
    val encoded = encoder("BGREWRITEAOF").result
  }

  private final class Bgsave(schedule: Boolean) extends RedisSimpleStringCommand with NodeCommand {
    val encoded = encoder("BGSAVE").addFlag("SCHEDULE", schedule).result
  }

  private final class ClientKill(address: ClientAddress) extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("CLIENT", "KILL").add(address.toString).result
  }

  private final class ClientKillFiltered(filters: Seq[ClientFilter]) extends RedisIntCommand with NodeCommand {
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

  private object ClientList extends AbstractRedisCommand[Seq[ClientInfo]](bulkClientInfos) with NodeCommand {
    val encoded = encoder("CLIENT", "LIST").result
  }

  private final class ClientPause(timeout: Long) extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("CLIENT", "PAUSE").add(timeout).result
  }

  private abstract class AbstractCommandInfoCommand
    extends AbstractRedisCommand[Seq[CommandInfo]](multiBulkSeq(multiBulkCommandInfo)) with NodeCommand

  private object Command extends AbstractCommandInfoCommand {
    val encoded = encoder("COMMAND").result
  }

  private object CommandCount extends RedisIntCommand with NodeCommand {
    val encoded = encoder("COMMAND", "COUNT").result
  }

  private final class CommandGetkeys(command: TraversableOnce[ByteString]) extends RedisDataSeqCommand[Key] with NodeCommand {
    val encoded = encoder("COMMAND", "GETKEYS").add(command).result
  }

  private final class CommandInfoCommand(commandNames: Iterable[String]) extends AbstractCommandInfoCommand {
    val encoded = encoder("COMMAND", "INFO").add(commandNames).result
  }

  private final class ConfigGet(parameter: String)
    extends AbstractRedisCommand[Seq[(String, String)]](pairedMultiBulk(bulkUTF8, bulkUTF8)) with NodeCommand {
    val encoded = encoder("CONFIG", "GET").add(parameter).result
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

  private object DebugSegfault extends RedisNothingCommand with NodeCommand {
    val encoded = encoder("DEBUG", "SEGFAULT").result
  }

  private object Flushall extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("FLUSHALL").result
  }

  private object Flushdb extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("FLUSHDB").result
  }

  private final class Info[T >: FullRedisInfo <: RedisInfo](section: RedisInfoSection[T], implicitDefault: Boolean)
    extends AbstractRedisCommand[T](bulk(bs => new FullRedisInfo(bs.utf8String))) with NodeCommand {
    val encoded = encoder("INFO").setup(e => if (!implicitDefault || section != DefaultRedisInfo) e.add(section.name)).result
  }

  private object Lastsave extends RedisLongCommand with NodeCommand {
    val encoded = encoder("LASTSAVE").result
  }

  private object Role extends AbstractRedisCommand[RedisRole](multiBulkRedisRole) with NodeCommand {
    val encoded = encoder("ROLE").result
  }

  private object Save extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("SAVE").result
  }

  private final class Shutdown(modifier: Opt[ShutdownModifier]) extends RedisNothingCommand with NodeCommand {
    val encoded = encoder("SHUTDOWN").optAdd(modifier).result
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

  private final class SlowlogGet(count: Opt[Int])
    extends RedisSeqCommand[SlowlogEntry](multiBulkSlowlogEntry) with NodeCommand {
    val encoded = encoder("SLOWLOG", "GET").optAdd(count).result
  }

  private object SlowlogLen extends RedisLongCommand with NodeCommand {
    val encoded = encoder("SLOWLOG", "LEN").result
  }

  private object SlowlogReset extends RedisUnitCommand with NodeCommand {
    val encoded = encoder("SLOWLOG", "RESET").result
  }

  private object Time extends AbstractRedisCommand[RedisTimestamp](multiBulkRedisTimestamp) with NodeCommand {
    val encoded = encoder("TIME").result
  }
}

trait ConnectionServerApi extends NodeServerApi {
  /** [[http://redis.io/commands/client-getname CLIENT GETNAME]] */
  def clientGetname: Result[Opt[String]] =
    execute(ClientGetname)
  /** [[http://redis.io/commands/client-setname CLIENT SETNAME]] */
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
  case object Save extends ShutdownModifier("SAVE")
  case object Nosave extends ShutdownModifier("NOSAVE")
  val values: List[ShutdownModifier] = caseObjects
}

case class SlowlogEntry(id: Long, timestamp: Long, duration: Long, command: Seq[ByteString])

case class RedisTimestamp(seconds: Long, useconds: Long)
