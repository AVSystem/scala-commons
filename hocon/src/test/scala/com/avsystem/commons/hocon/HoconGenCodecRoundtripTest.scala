package com.avsystem.commons
package hocon

import com.avsystem.commons.hocon.HoconInputTest.CustomCodecsClass
import com.avsystem.commons.serialization.{GenCodecRoundtripTest, Input, Output}
import com.typesafe.config.{ConfigFactory, ConfigValue}

import java.time.{Duration, Period}
import scala.concurrent.duration.*

class HoconGenCodecRoundtripTest extends GenCodecRoundtripTest {
  type Raw = ConfigValue

  def writeToOutput(write: Output => Unit): ConfigValue = {
    var result: ConfigValue | Null = null
    write(new HoconOutput(result = _))
    result
  }

  def createInput(raw: ConfigValue): Input =
    new HoconInput(raw)

  test("custom codes class") {
    val value = CustomCodecsClass(
      duration = 1.minute,
      jDuration = Duration.ofMinutes(5),
      fileSize = SizeInBytes.`1KiB`,
      embeddedConfig = ConfigFactory.parseMap(JMap("something" -> "abc")),
      period = Period.ofWeeks(2),
      clazz = classOf[HoconGenCodecRoundtripTest],
      clazzMap = Map(classOf[HoconGenCodecRoundtripTest] -> "abc"),
    )
    testRoundtrip(value)
  }
}
