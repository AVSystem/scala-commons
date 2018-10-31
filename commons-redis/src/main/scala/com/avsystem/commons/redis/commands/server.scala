package com.avsystem.commons
package redis.commands

import akka.util.ByteString
import com.avsystem.commons.misc.{NamedEnum, NamedEnumCompanion}
import com.avsystem.commons.redis.CommandEncoder.CommandArg
import com.avsystem.commons.redis._
import com.avsystem.commons.redis.commands.ReplyDecoders._
import com.avsystem.commons.redis.protocol.BulkStringMsg

trait NodeServerApi extends KeyedApiSubset {
  /** Executes [[http://redis.io/commands/bgrewriteaof BGREWRITEAOF]] */
  def bgrewriteaof: Result[String] =
    execute(Bgrewriteaof)
  /** Executes [[http://redis.io/commands/bgsave BGSAVE]] */
  def bgsave: Result[String] = bgsave()
  /** Executes [[http://redis.io/commands/bgsave BGSAVE]] */
  def bgsave(schedule: Boolean = false): Result[String] =
    execute(new Bgsave(schedule))
  /** Executes [[http://redis.io/commands/client-id CLIENT ID]] */
  def clientId: Result[ClientId] =
    execute(ClientId)
  /** Executes [[http://redis.io/commands/client-kill CLIENT KILL]] */
  def clientKill(addr: ClientAddress): Result[Unit] =
    execute(new ClientKill(addr))
  /** Executes [[http://redis.io/commands/client-kill CLIENT KILL]] */
  def clientKill(filters: ClientFilter*): Result[Int] =
    execute(new ClientKillFiltered(filters))
  /** Executes [[http://redis.io/commands/client-list CLIENT LIST]] */
  def clientList: Result[Seq[ClientInfo]] =
    execute(ClientList)
  /** Executes [[http://redis.io/commands/client-pause CLIENT PAUSE]] */
  def clientPause(timeout: Long): Result[Unit] =
    execute(new ClientPause(timeout))
  /** Executes [[http://redis.io/commands/client-unblock CLIENT UNBLOCK]] */
  def clientUnblock(clientId: ClientId, modifier: OptArg[UnblockModifier] = OptArg.Empty): Result[Boolean] =
    execute(new ClientUnblock(clientId, modifier.toOpt))
  /** Executes [[http://redis.io/commands/command COMMAND]] */
  def command: Result[Seq[CommandInfo]] =
    execute(Command)
  /** Executes [[http://redis.io/commands/command-count COMMAND COUNT]] */
  def commandCount: Result[Int] =
    execute(CommandCount)
  /** Executes [[http://redis.io/commands/command-getkeys COMMAND GETKEYS]] */
  def commandGetkeys(command: RawCommand): Result[Seq[Key]] =
    execute(new CommandGetkeys(command.encoded.elements.iterator.map(_.string)))
  /** Executes [[http://redis.io/commands/command-getkeys COMMAND GETKEYS]] */
  def commandGetkeys(command: Seq[ByteString]): Result[Seq[Key]] =
    execute(new CommandGetkeys(command))
  /** Executes [[http://redis.io/commands/command-info COMMAND INFO]] */
  def commandInfo(commandName: String): Result[CommandInfo] =
    execute(new CommandInfoCommand(commandName.single).map(_.head))
  /** Executes [[http://redis.io/commands/command-info COMMAND INFO]] */
  def commandInfo(commandName: String, commandNames: String*): Result[Seq[CommandInfo]] =
    execute(new CommandInfoCommand(commandName +:: commandNames))
  /** Executes [[http://redis.io/commands/command-info COMMAND INFO]] */
  def commandInfo(commandNames: Seq[String]): Result[Seq[CommandInfo]] =
    execute(new CommandInfoCommand(commandNames))
  /** Executes [[http://redis.io/commands/config-get CONFIG GET]] */
  def configGet(parameter: String): Result[Seq[(String, String)]] =
    execute(new ConfigGet(parameter))
  /** Executes [[http://redis.io/commands/config-resetstat CONFIG RESETSTAT]] */
  def configResetstat: Result[Unit] =
    execute(ConfigResetstat)
  /** Executes [[http://redis.io/commands/config-rewrite CONFIG REWRITE]] */
  def configRewrite: Result[Unit] =
    execute(ConfigRewrite)
  /** Executes [[http://redis.io/commands/config-set CONFIG SET]] */
  def configSet(parameter: String, value: String): Result[Unit] =
    execute(new ConfigSet(parameter, value))
  /** Executes [[http://redis.io/commands/dbsize DBSIZE]] */
  def dbsize: Result[Long] =
    execute(Dbsize)
  /** Executes [[http://redis.io/commands/debug-segfault DEBUG SEGFAULT]] */
  def debugSegfault: Result[Nothing] =
    execute(DebugSegfault)
  /** Executes [[http://redis.io/commands/flushall FLUSHALL]] */
  def flushall: Result[Unit] = flushall()
  /** Executes [[http://redis.io/commands/flushall FLUSHALL]] */
  def flushall(async: Boolean = false): Result[Unit] =
    execute(new Flushall(async))
  /** Executes [[http://redis.io/commands/flushdb FLUSHDB]] */
  def flushdb: Result[Unit] = flushdb()
  /** Executes [[http://redis.io/commands/flushdb FLUSHDB]] */
  def flushdb(async: Boolean = false): Result[Unit] =
    execute(new Flushdb(async))
  /** Executes [[http://redis.io/commands/info INFO]] */
  def info: Result[DefaultRedisInfo] =
    execute(new Info(DefaultRedisInfo, implicitDefault = true))
  /** Executes [[http://redis.io/commands/info INFO]] */
  def info[T >: FullRedisInfo <: RedisInfo](section: RedisInfoSection[T]): Result[T] =
    execute(new Info(section, implicitDefault = false))
  /** Executes [[http://redis.io/commands/lastsave LASTSAVE]] */
  def lastsave: Result[Long] =
    execute(Lastsave)
  /** Executes [[http://redis.io/commands/replicaof REPLICAOF]] */
  def replicaofNoOne: Result[Unit] =
    execute(new Replicaof(Opt.Empty))
  /** Executes [[http://redis.io/commands/replicaof REPLICAOF]] */
  def replicaof(newMaster: NodeAddress): Result[Unit] =
    execute(new Replicaof(newMaster.opt))
  /** Executes [[http://redis.io/commands/role ROLE]] */
  def role: Result[RedisRole] =
    execute(Role)
  /** Executes [[http://redis.io/commands/save SAVE]] */
  def save: Result[Unit] =
    execute(Save)
  /** Executes [[http://redis.io/commands/shutdown SHUTDOWN]] */
  def shutdown: Result[Nothing] = shutdown()
  /** Executes [[http://redis.io/commands/shutdown SHUTDOWN]] */
  def shutdown(modifier: OptArg[ShutdownModifier] = OptArg.Empty): Result[Nothing] =
    execute(new Shutdown(modifier.toOpt))
  /** Executes [[http://redis.io/commands/slaveof SLAVEOF]] */
  def slaveofNoOne: Result[Unit] =
    execute(new Slaveof(Opt.Empty))
  /** Executes [[http://redis.io/commands/slaveof SLAVEOF]] */
  def slaveof(newMaster: NodeAddress): Result[Unit] =
    execute(new Slaveof(newMaster.opt))
  /** Executes [[http://redis.io/commands/slowlog SLOWLOG]] */
  def slowlogGet: Result[Seq[SlowlogEntry]] = slowlogGet()
  /** Executes [[http://redis.io/commands/slowlog SLOWLOG]] */
  def slowlogGet(count: OptArg[Int] = OptArg.Empty): Result[Seq[SlowlogEntry]] =
    execute(new SlowlogGet(count.toOpt))
  /** Executes [[http://redis.io/commands/slowlog SLOWLOG]] */
  def slowlogLen: Result[Long] =
    execute(SlowlogLen)
  /** Executes [[http://redis.io/commands/slowlog SLOWLOG]] */
  def slowlogReset: Result[Unit] =
    execute(SlowlogReset)
  /** Executes [[http://redis.io/commands/time TIME]] */
  def time: Result[RedisTimestamp] =
    execute(Time)

  private object Bgrewriteaof extends RedisSimpleStringCommand with NodeCommand {
    val encoded: Encoded = encoder("BGREWRITEAOF").result
  }

  private final class Bgsave(schedule: Boolean) extends RedisSimpleStringCommand with NodeCommand {
    val encoded: Encoded = encoder("BGSAVE").addFlag("SCHEDULE", schedule).result
  }

  private object ClientId extends AbstractRedisCommand[ClientId](integerClientId) with NodeCommand {
    val encoded: Encoded = encoder("CLIENT", "ID").result
  }

  private final class ClientKill(address: ClientAddress) extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("CLIENT", "KILL").add(address.toString).result
  }

  private final class ClientKillFiltered(filters: Seq[ClientFilter]) extends RedisIntCommand with NodeCommand {
    val encoded: Encoded = {
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
    val encoded: Encoded = encoder("CLIENT", "LIST").result
  }

  private final class ClientPause(timeout: Long) extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("CLIENT", "PAUSE").add(timeout).result
  }

  private final class ClientUnblock(clientId: ClientId, modifier: Opt[UnblockModifier])
    extends RedisBooleanCommand with NodeCommand {
    val encoded: Encoded = encoder("CLIENT", "UNBLOCK").add(clientId.raw).optAdd(modifier).result
  }

  private abstract class AbstractCommandInfoCommand
    extends AbstractRedisCommand[Seq[CommandInfo]](multiBulkSeq(multiBulkCommandInfo)) with NodeCommand

  private object Command extends AbstractCommandInfoCommand {
    val encoded: Encoded = encoder("COMMAND").result
  }

  private object CommandCount extends RedisIntCommand with NodeCommand {
    val encoded: Encoded = encoder("COMMAND", "COUNT").result
  }

  private final class CommandGetkeys(command: TraversableOnce[ByteString]) extends RedisDataSeqCommand[Key] with NodeCommand {
    val encoded: Encoded = encoder("COMMAND", "GETKEYS").add(command).result
  }

  private final class CommandInfoCommand(commandNames: Iterable[String]) extends AbstractCommandInfoCommand {
    val encoded: Encoded = encoder("COMMAND", "INFO").add(commandNames).result
  }

  private final class ConfigGet(parameter: String)
    extends AbstractRedisCommand[Seq[(String, String)]](flatMultiBulkSeq(bulkUTF8, bulkUTF8)) with NodeCommand {
    val encoded: Encoded = encoder("CONFIG", "GET").add(parameter).result
  }

  private object ConfigResetstat extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("CONFIG", "RESETSTAT").result
  }

  private object ConfigRewrite extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("CONFIG", "REWRITE").result
  }

  private final class ConfigSet(parameter: String, value: String) extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("CONFIG", "SET").add(parameter).add(value).result
  }

  private object Dbsize extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("DBSIZE").result
  }

  private object DebugSegfault extends RedisNothingCommand with NodeCommand {
    val encoded: Encoded = encoder("DEBUG", "SEGFAULT").result
  }

  private final class Flushall(async: Boolean) extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("FLUSHALL").addFlag("ASYNC", async).result
  }

  private final class Flushdb(async: Boolean) extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("FLUSHDB").addFlag("ASYNC", async).result
  }

  private final class Info[T >: FullRedisInfo <: RedisInfo](section: RedisInfoSection[T], implicitDefault: Boolean)
    extends AbstractRedisCommand[T](bulk(bs => new FullRedisInfo(bs.utf8String))) with NodeCommand {
    val encoded: Encoded = encoder("INFO").setup(e => if (!implicitDefault || section != DefaultRedisInfo) e.add(section.name)).result
  }

  private object Lastsave extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("LASTSAVE").result
  }

  private final class Replicaof(newMaster: Opt[NodeAddress]) extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = {
      val enc = encoder("REPLICAOF")
      newMaster match {
        case Opt.Empty => enc.add("NO").add("ONE")
        case Opt(NodeAddress(ip, port)) => enc.add(ip).add(port)
      }
      enc.result
    }
  }

  private object Role extends AbstractRedisCommand[RedisRole](multiBulkRedisRole) with NodeCommand {
    val encoded: Encoded = encoder("ROLE").result
  }

  private object Save extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("SAVE").result
  }

  private final class Shutdown(modifier: Opt[ShutdownModifier]) extends RedisNothingCommand with NodeCommand {
    val encoded: Encoded = encoder("SHUTDOWN").optAdd(modifier).result
  }

  private final class Slaveof(newMaster: Opt[NodeAddress]) extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = {
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
    val encoded: Encoded = encoder("SLOWLOG", "GET").optAdd(count).result
  }

  private object SlowlogLen extends RedisLongCommand with NodeCommand {
    val encoded: Encoded = encoder("SLOWLOG", "LEN").result
  }

  private object SlowlogReset extends RedisUnitCommand with NodeCommand {
    val encoded: Encoded = encoder("SLOWLOG", "RESET").result
  }

  private object Time extends AbstractRedisCommand[RedisTimestamp](multiBulkRedisTimestamp) with NodeCommand {
    val encoded: Encoded = encoder("TIME").result
  }
}

trait ConnectionServerApi extends NodeServerApi {
  /** Executes [[http://redis.io/commands/client-getname CLIENT GETNAME]] */
  def clientGetname: Result[Opt[String]] =
    execute(ClientGetname)
  /** Executes [[http://redis.io/commands/client-setname CLIENT SETNAME]] */
  def clientSetname(connectionName: String): Result[Unit] =
    execute(new ClientSetname(connectionName))

  private object ClientGetname extends RedisOptStringCommand with ConnectionCommand {
    val encoded: Encoded = encoder("CLIENT", "GETNAME").result
  }

  private final class ClientSetname(connectionName: String) extends RedisUnitCommand with ConnectionCommand {
    val encoded: Encoded = encoder("CLIENT", "SETNAME").add(connectionName).result
  }
}

sealed trait ClientFilter extends Any
case class ClientId(raw: Long) extends AnyVal with ClientFilter {
  override def toString: String = java.lang.Long.toUnsignedString(raw)
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

  def slaveMonitor: Boolean = (this & O) != NoFlags
  def slave: Boolean = (this & S) != NoFlags
  def master: Boolean = (this & M) != NoFlags
  def transaction: Boolean = (this & x) != NoFlags
  def waitingBlocking: Boolean = (this & b) != NoFlags
  def waitingVMIO: Boolean = (this & i) != NoFlags
  def dirty: Boolean = (this & d) != NoFlags
  def closingAfterReply: Boolean = (this & c) != NoFlags
  def unblocked: Boolean = (this & u) != NoFlags
  def unixSocket: Boolean = (this & U) != NoFlags
  def clusterReadonly: Boolean = (this & r) != NoFlags
  def closingASAP: Boolean = (this & A) != NoFlags

  override def toString: String =
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

  def readable: Boolean = (this & r) != NoEvents
  def writable: Boolean = (this & w) != NoEvents

  override def toString: String =
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

  def id: ClientId = ClientId(attrMap("id"))
  def addr: ClientAddress = ClientAddress(attrMap("addr"))
  def fd: Int = attrMap("fd").toInt
  def name: String = attrMap("name")
  def age: Long = attrMap("age").toLong
  def idle: Long = attrMap("idle").toLong
  def flags: ClientFlags = ClientFlags(attrMap("flags"))
  def db: Int = attrMap("db").toInt
  def sub: Int = attrMap("sub").toInt
  def psub: Int = attrMap("psub").toInt
  def multi: Int = attrMap("multi").toInt
  def qbuf: Long = attrMap("qbuf").toLong
  def qbufFree: Long = attrMap("qbuf-free").toLong
  def obl: Long = attrMap("obl").toLong
  def oll: Long = attrMap("oll").toLong
  def omem: Long = attrMap("omem").toLong
  def events: ClientEvents = ClientEvents(attrMap("events"))
  def cmd: Opt[String] = attrMap("cmd").opt.filter(_ != "NULL")

  override def toString: String = infoLine
}

sealed abstract class UnblockModifier(val name: String) extends NamedEnum
object UnblockModifier extends NamedEnumCompanion[UnblockModifier] {
  object Timeout extends UnblockModifier("TIMEOUT")
  object Error extends UnblockModifier("ERROR")
  val values: List[UnblockModifier] = caseObjects
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

  def write: Boolean = (this & Write) != NoFlags
  def readonly: Boolean = (this & Readonly) != NoFlags
  def denyoom: Boolean = (this & Denyoom) != NoFlags
  def admin: Boolean = (this & Admin) != NoFlags
  def pubsub: Boolean = (this & Pubsub) != NoFlags
  def noscript: Boolean = (this & Noscript) != NoFlags
  def random: Boolean = (this & Random) != NoFlags
  def sortForScript: Boolean = (this & SortForScript) != NoFlags
  def loading: Boolean = (this & Loading) != NoFlags
  def stale: Boolean = (this & Stale) != NoFlags
  def skipMonitor: Boolean = (this & SkipMonitor) != NoFlags
  def asking: Boolean = (this & Asking) != NoFlags
  def fast: Boolean = (this & Fast) != NoFlags
  def movablekeys: Boolean = (this & Movablekeys) != NoFlags

  override def toString: String =
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

case class SlowlogEntry(id: Long, timestamp: Long, duration: Long, command: Seq[ByteString],
  clientAddress: Opt[ClientAddress] = Opt.Empty, clientName: Opt[String] = Opt.Empty)

case class RedisTimestamp(seconds: Long, useconds: Long)
