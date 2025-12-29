package com.avsystem.commons
package ser

import com.avsystem.commons.serialization.json.{JsonStringInput, JsonStringOutput}
import com.avsystem.commons.serialization.nativejs.{NativeJsonInput, NativeJsonOutput}
import japgolly.scalajs.benchmark.gui.GuiSuite
import japgolly.scalajs.benchmark.{Benchmark, Suite}

object JsonBenchmarks {
  val suite = GuiSuite(
    Suite("JSON serialization benchmarks")(
      Benchmark("Writing case class: GenCodec, String Json format") {
        JsonStringOutput.write(Something.Example)
      },
      Benchmark("Writing case class: GenCodec, Native Json format") {
        NativeJsonOutput.writeAsString(Something.Example)
      },
      Benchmark("Reading case class: GenCodec, String Json format") {
        JsonStringInput.read[Something](Something.ExampleJsonString)
      },
      Benchmark("Reading case class: GenCodec, Native Json format") {
        NativeJsonInput.readString[Something](Something.ExampleJsonString)
      },

      Benchmark("Writing sealed hierarchy: GenCodec, String Json format") {
        JsonStringOutput.write(SealedStuff.ExampleList)
      },
      Benchmark("Writing sealed hierarchy: GenCodec (flat), String Json format") {
        JsonStringOutput.write(FlatSealedStuff.ExampleList)
      },
      Benchmark("Writing sealed hierarchy: GenCodec, Native Json format") {
        NativeJsonOutput.writeAsString(SealedStuff.ExampleList)
      },
      Benchmark("Writing sealed hierarchy: GenCodec (flat), Native Json format") {
        NativeJsonOutput.writeAsString(FlatSealedStuff.ExampleList)
      },
      Benchmark("Reading sealed hierarchy: GenCodec, String Json format") {
        JsonStringInput.read[List[SealedStuff]](SealedStuff.ExampleJsonString)
      },
      Benchmark("Reading sealed hierarchy: GenCodec (flat), String Json format") {
        JsonStringInput.read[List[FlatSealedStuff]](FlatSealedStuff.ExampleJsonString)
      },
      Benchmark("Reading sealed hierarchy: GenCodec, Native Json format") {
        NativeJsonInput.readString[List[SealedStuff]](SealedStuff.ExampleJsonString)
      },
      Benchmark("Reading sealed hierarchy: GenCodec (flat), Native Json format") {
        NativeJsonInput.readString[List[FlatSealedStuff]](FlatSealedStuff.ExampleJsonString)
      },

      Benchmark("Writing foos: GenCodec, String Json format") {
        JsonStringOutput.write(Foo.ExampleMap)
      },
      Benchmark("Writing foos: GenCodec, Native Json format") {
        NativeJsonOutput.writeAsString(Foo.ExampleMap)
      },
      Benchmark("Reading foos: GenCodec, String Json format") {
        JsonStringInput.read[Map[String, Foo]](Foo.ExampleJsonString)
      },
      Benchmark("Reading foos: GenCodec with Native Json format") {
        NativeJsonInput.readString[Map[String, Foo]](Foo.ExampleJsonString)
      },
    )
  )
}
