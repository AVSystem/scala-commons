package com.avsystem.commons
package redis

import akka.util.ByteString
import com.avsystem.commons.redis.actor.RedisConnectionActor.DebugListener
import com.avsystem.commons.redis.protocol.RedisMsg

/**
  * Author: ghik
  * Created: 29/07/16.
  */
class TestDebugListener extends DebugListener {
  private var receiving = false
  private val builder = new StringBuilder

  def clear(): Unit = synchronized {
    receiving = true
    builder.clear()
  }

  def onSend(data: ByteString) = synchronized {
    if (receiving) {
      builder.append("\n")
      receiving = false
    }
    builder.append(RedisMsg.escape(data, quote = false).replace("\\r\\n", "\\r\\n\n"))
  }

  def onReceive(data: ByteString) = synchronized {
    if (!receiving) {
      builder.append("\n")
      receiving = true
    }
    builder.append(RedisMsg.escape(data, quote = false).replace("\\r\\n", "\\r\\n\n"))
  }

  def result(): String = synchronized {
    builder.result()
  }
}
