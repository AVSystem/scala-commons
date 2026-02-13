package com.avsystem.commons
package ser

import com.avsystem.commons.serialization.GenCodec.ReadFailure
import japgolly.scalajs.benchmark.gui.GuiSuite
import japgolly.scalajs.benchmark.{Benchmark, Suite}

import scala.scalajs.js
import scala.scalajs.js.RegExp

object IsoInstantBenchmarks {
  private val regex: RegExp =
    js.RegExp("""^(\+|-)?[0-9]+-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\.[0-9]{3})?Z$""")

  def parse(string: String, validate: Boolean): Long = {
    def fail = throw new ReadFailure(s"invalid ISO instant: $string")
    if (!validate || regex.test(string)) {
      val parsed = js.Date.parse(string)
      if (parsed.isNaN) fail
      else parsed.toLong
    } else fail
  }

  val suite = GuiSuite(
    Suite("IsoInstant parsing")(
      Benchmark("with regex validation") {
        parse("2013-11-27T12:55:32.234Z", validate = true)
      },
      Benchmark("without regex validation") {
        parse("2013-11-27T12:55:32.234Z", validate = false)
      },
    )
  )
}
