package com.avsystem.commons
package hocon

import java.time.{Duration, Period}

import com.avsystem.commons.serialization.json.JsonStringOutput
import com.avsystem.commons.serialization.{GenCodecRoundtripTest, Input, Output}
import com.typesafe.config.{ConfigFactory, ConfigMemorySize, ConfigValue, ConfigValueFactory, ConfigValueType}

class HoconInputTest extends GenCodecRoundtripTest {
  type Raw = ConfigValue

  def writeToOutput(write: Output => Unit): ConfigValue = {
    val sb = new JStringBuilder
    write(new JsonStringOutput(sb))
    val config = ConfigFactory.parseString(s"""{"f":${sb.toString}}""")
    if (config.getIsNull("f")) ConfigValueFactory.fromAnyRef(null) else config.getValue("f")
  }

  def createInput(raw: ConfigValue): Input =
    new HoconInput(raw)

  def rawInput(any: Any): HoconInput =
    new HoconInput(ConfigValueFactory.fromAnyRef(any))

  test("value type reading") {
    assert(rawInput(null).valueType == ConfigValueType.NULL)
    assert(rawInput("kek").valueType == ConfigValueType.STRING)
    assert(rawInput(42).valueType == ConfigValueType.NUMBER)
    assert(rawInput(true).valueType == ConfigValueType.BOOLEAN)
    assert(rawInput(JMap()).valueType == ConfigValueType.OBJECT)
    assert(rawInput(JList()).valueType == ConfigValueType.LIST)
  }

  test("duration reading") {
    assert(rawInput("34s").readDuration() == Duration.ofSeconds(34))
  }

  test("period reading") {
    assert(rawInput("5m").readPeriod() == Period.ofMonths(5))
  }

  test("temporal amount reading") {
    assert(rawInput("5 minutes").readTemporal() == Duration.ofMinutes(5))
    assert(rawInput("5 months").readTemporal() == Period.ofMonths(5))
  }

  test("size in bytes reading") {
    assert(rawInput("100M").readSizeInBytes() == 100 * 1024 * 1024L)
  }

  test("memory size reading") {
    assert(rawInput("100M").readMemorySize() == ConfigMemorySize.ofBytes(100 * 1024 * 1024L))
  }

  test("number reading") {
    assert(rawInput(42.0).readNumber().doubleValue == 42.0)
  }
}
