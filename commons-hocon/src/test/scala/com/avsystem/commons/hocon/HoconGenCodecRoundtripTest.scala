package com.avsystem.commons
package hocon

import com.avsystem.commons.serialization.{GenCodecRoundtripTest, Input, Output}
import com.typesafe.config.ConfigValue

class HoconGenCodecRoundtripTest extends GenCodecRoundtripTest {
  type Raw = ConfigValue

  def writeToOutput(write: Output => Unit): ConfigValue = {
    var result: ConfigValue = null
    write(new HoconOutput(result = _))
    result
  }

  def createInput(raw: ConfigValue): Input =
    new HoconInput(raw)
}
