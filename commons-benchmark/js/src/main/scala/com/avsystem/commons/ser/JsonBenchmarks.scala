package com.avsystem.commons
package ser

import com.avsystem.commons.serialization.json.{JsonStringInput, JsonStringOutput}
import io.circe.parser._
import io.circe.syntax._
import japgolly.scalajs.benchmark.gui.GuiSuite
import japgolly.scalajs.benchmark.{Benchmark, Suite}

object JsonBenchmarks {
  val suite = GuiSuite(
    Suite("JSON serialization benchmarks")(
      Benchmark("Writing case class: GenCodec") {
        JsonStringOutput.write(Something.Example)
      },
      Benchmark("Writing case class: Circe") {
        Something.Example.asJson.noSpaces
      },
      Benchmark("Writing case class: uPickle") {
        upickle.default.write(Something.Example)
      },
      Benchmark("Reading case class: GenCodec") {
        JsonStringInput.read[Something](Something.ExampleJsonString)
      },
      Benchmark("Reading case class: Circe") {
        decode[Something](Something.ExampleJsonString).fold(e => throw e, identity)
      },
      Benchmark("Reading case class: uPickle") {
        upickle.default.read[Something](Something.ExampleJsonString)
      },

      Benchmark("Writing sealed hierarchy: GenCodec") {
        JsonStringOutput.write(SealedStuff.ExampleList)
      },
      Benchmark("Writing sealed hierarchy: GenCodec (flat)") {
        JsonStringOutput.write(FlatSealedStuff.ExampleList)
      },
      Benchmark("Writing sealed hierarchy: Circe") {
        SealedStuff.ExampleList.asJson.noSpaces
      },
      Benchmark("Writing sealed hierarchy: uPickle") {
        upickle.default.write(SealedStuff.ExampleList)
      },
      Benchmark("Reading sealed hierarchy: GenCodec") {
        JsonStringInput.read[List[SealedStuff]](SealedStuff.ExampleJsonString)
      },
      Benchmark("Reading sealed hierarchy: GenCodec (flat)") {
        JsonStringInput.read[List[FlatSealedStuff]](FlatSealedStuff.ExampleJsonString)
      },
      Benchmark("Reading sealed hierarchy: Circe") {
        decode[List[SealedStuff]](SealedStuff.ExampleJsonString).fold(e => throw e, identity)
      },
      Benchmark("Reading sealed hierarchy: uPickle") {
        upickle.default.read[List[SealedStuff]](SealedStuff.ExampleUpickleJsonString)
      },

      Benchmark("Writing foos: GenCodec") {
        JsonStringOutput.write(Foo.ExampleMap)
      },
      Benchmark("Writing foos: Circe") {
        Foo.ExampleMap.asJson.noSpaces
      },
      Benchmark("Writing foos: uPickle") {
        upickle.default.write(Foo.ExampleMap)
      },
      Benchmark("Reading foos: GenCodec") {
        JsonStringInput.read[Map[String, Foo]](Foo.ExampleJsonString)
      },
      Benchmark("Reading foos: Circe") {
        decode[Map[String, Foo]](Foo.ExampleJsonString).fold(e => throw e, identity)
      },
      Benchmark("Reading foos: uPickle") {
        upickle.default.read[Map[String, Foo]](Foo.ExampleUpickleJsonString)
      }
    )
  )
}
