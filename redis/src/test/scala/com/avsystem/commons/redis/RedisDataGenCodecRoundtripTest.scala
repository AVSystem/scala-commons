package com.avsystem.commons
package redis

import akka.util.ByteString
import com.avsystem.commons.serialization.{GenCodecRoundtripTest, Input, Output}

class RedisDataGenCodecRoundtripTest extends GenCodecRoundtripTest {
  type Raw = ByteString

  def writeToOutput(write: Output => Unit): ByteString = {
    var result: ByteString = null
    write(new RedisDataOutput(result = _))
    result
  }

  def createInput(raw: ByteString): Input =
    new RedisDataInput(raw)
}
