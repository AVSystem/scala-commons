package com.avsystem.commons
package hocon

import java.time.temporal.TemporalAmount
import java.time.{Duration, Period}
import java.util.Base64

import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization._
import com.typesafe.config._

import scala.concurrent.duration.FiniteDuration

object ConfigInput {
  def read[T: GenCodec](value: ConfigValue): T =
    GenCodec.read[T](new ConfigInput(value))

  def read[T: GenCodec](config: Config): T =
    GenCodec.read[T](new ConfigInput(config.root))
}

trait BaseConfigInput {
  protected final def handleFailures[T](code: => T): T =
    try code.opt.getOrElse(throw new ReadFailure("null")) catch {
      case rf: ReadFailure => throw rf
      case NonFatal(cause) => throw new ReadFailure(cause.getMessage, cause)
    }
}

class ConfigInput(value: ConfigValue) extends InputAndSimpleInput with BaseConfigInput {
  // For wrapping ConfigValue into Config in order to use its rich API
  // Why is this API not available directly on ConfigValue?
  private def FakePath = "fake"

  private def config: Config =
    ConfigFactory.empty.withValue(FakePath, value)

  def readNull(): Boolean = handleFailures(config.getIsNull(FakePath))
  def readList(): ListInput = handleFailures(new ConfigListInput(config.getList(FakePath)))
  def readObject(): ObjectInput = handleFailures(new ConfigObjectInput(config.getObject(FakePath)))
  def skip(): Unit = ()

  def readString(): String = handleFailures(config.getString(FakePath))
  def readBoolean(): Boolean = handleFailures(config.getBoolean(FakePath))
  def readInt(): Int = handleFailures(config.getInt(FakePath))
  def readLong(): Long = handleFailures(config.getLong(FakePath))
  def readDouble(): Double = handleFailures(config.getDouble(FakePath))
  def readBigInt(): BigInt = handleFailures(BigInt(config.getString(FakePath)))
  def readBigDecimal(): BigDecimal = handleFailures(BigDecimal(config.getString(FakePath)))
  def readBinary(): Array[Byte] = handleFailures(Base64.getDecoder.decode(config.getString(FakePath)))

  def readValue(): ConfigValue = value
  def valueType: ConfigValueType = value.valueType

  override def readMetadata[T](metadata: InputMetadata[T]): Opt[T] = metadata match {
    case ConfigValueTypeMarker => valueType.opt
    case _ => super.readMetadata(metadata)
  }

  override def readCustom[T](typeMarker: TypeMarker[T]): Opt[T] = handleFailures {
    typeMarker match {
      case ConfigValueMarker =>
        readValue().opt
      case DurationMarker =>
        Opt(config.getDuration(FakePath))
      case SizeInBytesMarker =>
        Opt(config.getBytes(FakePath))
      case ConfigMemorySizeMarker =>
        Opt(config.getMemorySize(FakePath))
      case PeriodMarker =>
        Opt(config.getPeriod(FakePath))
      case TemporalAmountMarker =>
        Opt(config.getTemporal(FakePath))
      case NumberMarker =>
        Opt(config.getNumber(FakePath))
      case _ => super.readCustom(typeMarker)
    }
  }
}

class ConfigListInput(configList: ConfigList) extends ListInput with BaseConfigInput {
  private val elements = configList.iterator.asScala

  def hasNext: Boolean = elements.hasNext

  def nextElement(): Input =
    handleFailures(new ConfigInput(elements.next()))
}

class ConfigObjectInput(configObject: ConfigObject) extends ObjectInput with BaseConfigInput {
  private val keys = configObject.keySet.iterator.asScala

  def hasNext: Boolean = keys.hasNext

  def nextField(): FieldInput = handleFailures {
    val nextKey = keys.next()
    new ConfigFieldInput(nextKey, configObject.get(nextKey))
  }

  override def peekField(name: String): Opt[FieldInput] =
    if (configObject.containsKey(name))
      new ConfigFieldInput(name, configObject.get(name)).opt
    else Opt.Empty
}

class ConfigFieldInput(val fieldName: String, value: ConfigValue)
  extends ConfigInput(value) with FieldInput

/**
  * [[com.avsystem.commons.serialization.InputMetadata InputMetadata]] marker object which allows inspection of
  * `com.typesafe.config.ConfigValueType` on a [[ConfigInput]] in a
  * [[com.avsystem.commons.serialization.GenCodec GenCodec]] implementation.
  */
object ConfigValueTypeMarker extends InputMetadata[ConfigValueType]

/**
  * [[com.avsystem.commons.serialization.TypeMarker TypeMarker]] which allows you to read raw
  * `com.typesafe.config.ConfigValue` from a [[ConfigInput]] in a
  * [[com.avsystem.commons.serialization.GenCodec GenCodec]] implementation.
  */
object ConfigValueMarker extends TypeMarker[ConfigValue]

/**
  * [[com.avsystem.commons.serialization.TypeMarker TypeMarker]] which allows direct reading of a `Duration` 
  * from [[ConfigInput]] in a [[com.avsystem.commons.serialization.GenCodec GenCodec]] implementation, taking advantage
  * of the [[https://github.com/lightbend/config/blob/master/HOCON.md#duration-format recommended HOCON format for durations]],
  * implemented by `Config.getDuration`.
  */
object DurationMarker extends TypeMarker[Duration]

/**
  * [[com.avsystem.commons.serialization.TypeMarker TypeMarker]] which allows direct reading of a size in bytes 
  * from [[ConfigInput]] in a [[com.avsystem.commons.serialization.GenCodec GenCodec]] implementation, taking advantage
  * of the [[https://github.com/lightbend/config/blob/master/HOCON.md#size-in-bytes-format HOCON format for size in bytes]],
  * implemented by `Config.getBytes`.
  */
object SizeInBytesMarker extends TypeMarker[Long]

/**
  * [[com.avsystem.commons.serialization.TypeMarker TypeMarker]] which allows direct reading of a `ConfigMemorySize` 
  * from [[ConfigInput]] in a [[com.avsystem.commons.serialization.GenCodec GenCodec]] implementation, taking advantage
  * of the [[https://github.com/lightbend/config/blob/master/HOCON.md#size-in-bytes-format HOCON format for size in bytes]],
  * implemented by `Config.getMemorySize`.
  */
object ConfigMemorySizeMarker extends TypeMarker[ConfigMemorySize]

/**
  * [[com.avsystem.commons.serialization.TypeMarker TypeMarker]] which allows direct reading of a `Period` 
  * from [[ConfigInput]] in a [[com.avsystem.commons.serialization.GenCodec GenCodec]] implementation, taking advantage
  * of the [[https://github.com/lightbend/config/blob/master/HOCON.md#period-format HOCON format for period]],
  * implemented by `Config.getPeriod`.
  */
object PeriodMarker extends TypeMarker[Period]

/**
  * [[com.avsystem.commons.serialization.TypeMarker TypeMarker]] which allows direct reading of a `TemporalAmount` 
  * from [[ConfigInput]] in a [[com.avsystem.commons.serialization.GenCodec GenCodec]] implementation, taking advantage
  * of the [[https://github.com/lightbend/config/blob/master/HOCON.md#period-format HOCON format for period]],
  * implemented by `Config.getTemporal`.
  */
object TemporalAmountMarker extends TypeMarker[TemporalAmount]

/**
  * [[com.avsystem.commons.serialization.TypeMarker TypeMarker]] which allows direct reading of a `java.lang.Number`
  * from [[ConfigInput]] in a [[com.avsystem.commons.serialization.GenCodec GenCodec]] implementation, taking advantage
  * of the parsing implemented by `Config.getNumber`.
  */
object NumberMarker extends TypeMarker[Number]
