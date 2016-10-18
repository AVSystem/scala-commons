package com.avsystem.commons
package redis

import scala.concurrent.duration.Duration

/**
  * Author: ghik
  * Created: 08/09/16.
  */
object Profiled {
  val bench = new RedisClientBenchmark

  def main(args: Array[String]): Unit = {
    while (true) {
      bench.nodeClientMixedBenchmark()
    }
  }
}
