package com.avsystem.commons
package hocon

import java.time.temporal.TemporalAmount
import java.time.{Duration, Period}
import java.util.Base64

import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization._
import com.typesafe.config._

object HoconInput {
  def read[T: GenCodec](value: ConfigValue): T =
    GenCodec.read[T](new HoconInput(value))

  def read[T: GenCodec](config: Config): T =
    GenCodec.read[T](new HoconInput(config.root))
}

trait BaseHoconInput {
  protected final def handleFailures[T](code: => T): T =
    try code.opt.getOrElse(throw new ReadFailure("null")) catch {
      case rf: ReadFailure => throw rf
      case NonFatal(cause) => throw new ReadFailure(cause.getMessage, cause)
    }
}

class HoconInput(value: ConfigValue) extends InputAndSimpleInput with BaseHoconInput {
  // For wrapping ConfigValue into Config in order to use its rich API
  // Why is this API not available directly on ConfigValue?
  private def FakePath = "fake"

  private def config: Config =
    ConfigFactory.empty.withValue(FakePath, value)

  def readNull(): Boolean = handleFailures(config.getIsNull(FakePath))
  def readList(): ListInput = handleFailures(new HoconListInput(config.getList(FakePath)))
  def readObject(): ObjectInput = handleFailures(new HoconObjectInput(config.getObject(FakePath)))
  def skip(): Unit = ()

  def readString(): String = handleFailures(config.getString(FakePath))
  def readBoolean(): Boolean = handleFailures(config.getBoolean(FakePath))
  def readInt(): Int = handleFailures(config.getInt(FakePath))
  def readLong(): Long = handleFailures(config.getLong(FakePath))
  def readDouble(): Double = handleFailures(config.getDouble(FakePath))
  def readBigInt(): BigInt = handleFailures(BigInt(config.getString(FakePath)))
  def readBigDecimal(): BigDecimal = handleFailures(BigDecimal(config.getString(FakePath)))
  def readBinary(): Array[Byte] = handleFailures(Base64.getDecoder.decode(config.getString(FakePath)))

  // HOCON-only extensions
  def readValue(): ConfigValue = value
  def readDuration(): Duration = handleFailures(config.getDuration(FakePath))
  def readSizeInBytes(): Long = handleFailures(config.getBytes(FakePath))
  def readMemorySize(): ConfigMemorySize = handleFailures(config.getMemorySize(FakePath))
  def readPeriod(): Period = handleFailures(config.getPeriod(FakePath))
  def readTemporal(): TemporalAmount = handleFailures(config.getTemporal(FakePath))
  def readNumber(): Number = handleFailures(config.getNumber(FakePath))

  def valueType: ConfigValueType = value.valueType

  override def readMetadata[T](metadata: InputMetadata[T]): Opt[T] = metadata match {
    case ConfigValueTypeMarker => valueType.opt
    case _ => super.readMetadata(metadata)
  }

  override def readCustom[T](typeMarker: TypeMarker[T]): Opt[T] = typeMarker match {
    case hoconTypeMarker: HoconTypeMarker[T] => hoconTypeMarker match {
      case ConfigValueMarker => Opt(readValue())
      case DurationMarker => Opt(readDuration())
      case SizeInBytesMarker => Opt(readSizeInBytes())
      case ConfigMemorySizeMarker => Opt(readMemorySize())
      case PeriodMarker => Opt(readPeriod())
      case TemporalAmountMarker => Opt(readTemporal())
      case NumberMarker => Opt(readNumber())
    }
    case _ => super.readCustom(typeMarker)
  }
}

class HoconListInput(configList: ConfigList) extends ListInput with BaseHoconInput {
  private val elements = configList.iterator.asScala

  def hasNext: Boolean = elements.hasNext

  def nextElement(): Input =
    handleFailures(new HoconInput(elements.next()))
}

class HoconObjectInput(configObject: ConfigObject) extends ObjectInput with BaseHoconInput {
  private val keys = configObject.keySet.iterator.asScala

  def hasNext: Boolean = keys.hasNext

  def nextField(): FieldInput = handleFailures {
    val nextKey = keys.next()
    new HoconFieldInput(nextKey, configObject.get(nextKey))
  }

  override def peekField(name: String): Opt[FieldInput] =
    if (configObject.containsKey(name))
      new HoconFieldInput(name, configObject.get(name)).opt
    else Opt.Empty
}

class HoconFieldInput(val fieldName: String, value: ConfigValue)
  extends HoconInput(value) with FieldInput

/**
  * [[com.avsystem.commons.serialization.InputMetadata InputMetadata]] marker object which allows inspection of
  * `com.typesafe.config.ConfigValueType` on a [[HoconInput]] in a
  * [[com.avsystem.commons.serialization.GenCodec GenCodec]] implementation.
  */
object ConfigValueTypeMarker extends InputMetadata[ConfigValueType]

sealed trait HoconTypeMarker[T] extends TypeMarker[T]

/**
  * [[com.avsystem.commons.serialization.TypeMarker TypeMarker]] which allows you to read raw
  * `com.typesafe.config.ConfigValue` from a [[HoconInput]] in a
  * [[com.avsystem.commons.serialization.GenCodec GenCodec]] implementation.
  */
object ConfigValueMarker extends HoconTypeMarker[ConfigValue]

/**
  * [[com.avsystem.commons.serialization.TypeMarker TypeMarker]] which allows direct reading of a `Duration`
  * from [[HoconInput]] in a [[com.avsystem.commons.serialization.GenCodec GenCodec]] implementation, taking advantage
  * of the [[https://github.com/lightbend/config/blob/master/HOCON.md#duration-format recommended HOCON format for durations]],
  * implemented by `Config.getDuration`.
  */
object DurationMarker extends HoconTypeMarker[Duration]

/**
  * [[com.avsystem.commons.serialization.TypeMarker TypeMarker]] which allows direct reading of a size in bytes
  * from [[HoconInput]] in a [[com.avsystem.commons.serialization.GenCodec GenCodec]] implementation, taking advantage
  * of the [[https://github.com/lightbend/config/blob/master/HOCON.md#size-in-bytes-format HOCON format for size in bytes]],
  * implemented by `Config.getBytes`.
  */
object SizeInBytesMarker extends HoconTypeMarker[Long]

/**
  * [[com.avsystem.commons.serialization.TypeMarker TypeMarker]] which allows direct reading of a `ConfigMemorySize`
  * from [[HoconInput]] in a [[com.avsystem.commons.serialization.GenCodec GenCodec]] implementation, taking advantage
  * of the [[https://github.com/lightbend/config/blob/master/HOCON.md#size-in-bytes-format HOCON format for size in bytes]],
  * implemented by `Config.getMemorySize`.
  */
object ConfigMemorySizeMarker extends HoconTypeMarker[ConfigMemorySize]

/**
  * [[com.avsystem.commons.serialization.TypeMarker TypeMarker]] which allows direct reading of a `Period`
  * from [[HoconInput]] in a [[com.avsystem.commons.serialization.GenCodec GenCodec]] implementation, taking advantage
  * of the [[https://github.com/lightbend/config/blob/master/HOCON.md#period-format HOCON format for period]],
  * implemented by `Config.getPeriod`.
  */
object PeriodMarker extends HoconTypeMarker[Period]

/**
  * [[com.avsystem.commons.serialization.TypeMarker TypeMarker]] which allows direct reading of a `TemporalAmount`
  * from [[HoconInput]] in a [[com.avsystem.commons.serialization.GenCodec GenCodec]] implementation, taking advantage
  * of the [[https://github.com/lightbend/config/blob/master/HOCON.md#period-format HOCON format for period]],
  * implemented by `Config.getTemporal`.
  */
object TemporalAmountMarker extends HoconTypeMarker[TemporalAmount]

/**
  * [[com.avsystem.commons.serialization.TypeMarker TypeMarker]] which allows direct reading of a `java.lang.Number`
  * from [[HoconInput]] in a [[com.avsystem.commons.serialization.GenCodec GenCodec]] implementation, taking advantage
  * of the parsing implemented by `Config.getNumber`.
  */
object NumberMarker extends HoconTypeMarker[Number]
