package com.avsystem.commons

import com.avsystem.commons.ser.{IsoInstantBenchmarks, JsonBenchmarks}
import japgolly.scalajs.benchmark.gui.BenchmarkGUI
import org.scalajs.dom._

object Main {
  def main(args: Array[String]): Unit = {
    val body = document.getElementById("body")
    BenchmarkGUI.renderMenu(body)(
      IsoInstantBenchmarks.suite,
      JsonBenchmarks.suite
    )
  }
}
