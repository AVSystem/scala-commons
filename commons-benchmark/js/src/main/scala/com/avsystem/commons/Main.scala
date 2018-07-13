package com.avsystem.commons

import com.avsystem.commons.ser.IsoInstantBenchmarks
import japgolly.scalajs.benchmark.gui.BenchmarkGUI
import org.scalajs.dom._

object Main {
  def main(args: Array[String]): Unit = {
    val body = document.getElementById("body")
    BenchmarkGUI.renderSuite(body)(IsoInstantBenchmarks.suite)
  }
}
