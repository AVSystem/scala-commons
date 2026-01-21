package com.avsystem.commons
package hocon

import com.avsystem.commons.annotation.explicitGenerics
import com.avsystem.commons.serialization.*
import com.avsystem.commons.serialization.cbor.RawCbor
import com.avsystem.commons.serialization.json.RawJson
import com.typesafe.config.{ConfigValue, ConfigValueFactory}

object HoconOutput {
  @explicitGenerics
  def write[T: GenCodec](value: T): ConfigValue = {
    var result: ConfigValue | Null = null
    GenCodec[T].write(new HoconOutput(result = _), value)
    result
  }
}

class HoconOutput(consumer: ConfigValue => Unit) extends OutputAndSimpleOutput {
  private def anyRef(value: Any): Unit =
    consumer(ConfigValueFactory.fromAnyRef(value.asInstanceOf[AnyRef]))

  def writeNull(): Unit = anyRef(null)
  def writeString(str: String): Unit = anyRef(str)
  def writeBoolean(boolean: Boolean): Unit = anyRef(boolean)
  def writeInt(int: Int): Unit = anyRef(int)
  def writeLong(long: Long): Unit = anyRef(long)
  def writeDouble(double: Double): Unit = anyRef(double)
  def writeBigInt(bigInt: BigInt): Unit = anyRef(bigInt.toString)
  def writeBigDecimal(bigDecimal: BigDecimal): Unit = anyRef(bigDecimal.toString)
  def writeBinary(binary: Array[Byte]): Unit = anyRef(Base64.encode(binary))

  def writeList(): HoconListOutput = new HoconListOutput(consumer)
  def writeObject(): HoconObjectOutput = new HoconObjectOutput(consumer)

  // TODO: handle other markers in writeCustom? Unfortunately typesafe config does not provide a methods to write
  // duration to nice string representation etc.
  override def writeCustom[T](typeMarker: TypeMarker[T], value: T): Boolean =
    typeMarker match {
      case ConfigValueMarker => consumer(value); true
      case PeriodMarker => anyRef(value.toString.toLowerCase.stripPrefix("p")); true
      case _ => false
    }
}

class HoconListOutput(consumer: ConfigValue => Unit) extends ListOutput {
  private val result = new JArrayList[ConfigValue]

  def writeElement(): Output = new HoconOutput(result.add(_))
  def finish(): Unit = consumer(ConfigValueFactory.fromIterable(result))
}

class HoconObjectOutput(consumer: ConfigValue => Unit) extends ObjectOutput {
  private val result = new JLinkedHashMap[String, ConfigValue]

  def writeField(key: String): Output = new HoconOutput(result.put(key, _))
  def finish(): Unit = consumer(ConfigValueFactory.fromMap(result))
}
