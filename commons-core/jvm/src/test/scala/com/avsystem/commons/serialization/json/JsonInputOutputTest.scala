package com.avsystem.commons
package serialization.json

import com.avsystem.commons.serialization.Output
import io.circe.testing.ArbitraryInstances
import io.circe.{Json, JsonNumber}
import org.scalatest.FunSuite
import org.scalatest.concurrent.Eventually
import org.scalatest.prop.PropertyChecks

import scala.collection.mutable.ListBuffer

class JsonInputOutputTest extends FunSuite with PropertyChecks with ArbitraryInstances with Eventually {
  // limit JsonNumbers to Int values
  override def transformJsonNumber(n: JsonNumber): JsonNumber =
    Json.fromInt(n.toBigDecimal.map(_.intValue).getOrElse(0)).asNumber.get

  private def write(json: Json): String = {
    def writeIn(json: Json, output: Output): Unit = json.fold(
      output.writeNull(),
      output.writeBoolean,
      num => num.toInt.fold(output.writeDouble(num.toDouble))(output.writeInt),
      output.writeString,
      arr => {
        val lo = output.writeList()
        arr.foreach(writeIn(_, lo.writeElement()))
        lo.finish()
      },
      obj => {
        val oo = output.writeObject()
        obj.toList.foreach {
          case (key, j) => writeIn(j, oo.writeField(key))
        }
        oo.finish()
      }
    )

    val builder = new JStringBuilder
    val output = new JsonStringOutput(builder)
    writeIn(json, output)
    builder.toString
  }

  private def read(json: String): Json = {
    def readIn(input: JsonStringInput): Json = input.jsonType match {
      case "null" => input.readNull(); Json.Null
      case "boolean" => Json.fromBoolean(input.readBoolean())
      case "string" => Json.fromString(input.readString())
      case "integer number" => Json.fromInt(input.readInt())
      case "decimal number" => Json.fromDouble(input.readDouble()).get
      case "list" =>
        val li = input.readList()
        val lb = new ListBuffer[Json]
        while (li.hasNext) {
          lb += readIn(li.nextElement())
        }
        Json.fromValues(lb)
      case "object" =>
        val oi = input.readObject()
        val lb = new ListBuffer[(String, Json)]
        while (oi.hasNext) {
          val fi = oi.nextField()
          lb += ((fi.fieldName, readIn(fi)))
        }
        Json.fromFields(lb)
    }

    readIn(new JsonStringInput(new JsonReader(json)))
  }

  // The property tests like to freeze for no apparent reason, hence the use of eventually
  test("write consistency with circe") {
    eventually {
      forAll { json: Json =>
        assert(write(json) == json.noSpaces)
      }
    }
  }

  test("read consistency with circe - compact") {
    eventually {
      forAll { json: Json =>
        assert(read(json.noSpaces) == json)
      }
    }
  }

  test("read consistency with circe - spaced") {
    eventually {
      forAll { json: Json =>
        assert(read(json.spaces2) == json)
      }
    }
  }

  test("read write round trip") {
    eventually {
      forAll { json: Json =>
        assert(read(write(json)) == json)
      }
    }
  }

  test("raw json list test") {
    val jsons = List("123", "null", "\"str\"", "4.5", "[1,2,3]", "{\"a\": 123, \"b\": 3.14}")
    val sb = new JStringBuilder
    val output = new JsonStringOutput(sb)
    val lo = output.writeList()
    jsons.foreach(json => lo.writeElement().writeRawJson(json))
    lo.finish()
    val jsonList = sb.toString

    assert(jsonList == jsons.mkString("[", ",", "]"))

    val input = new JsonStringInput(new JsonReader(jsonList))
    val li = input.readList()
    val resBuilder = new ListBuffer[String]
    while (li.hasNext) {
      resBuilder += li.nextElement().readRawJson()
    }
    assert(resBuilder.result() == jsons)
  }

  test("raw json object test") {
    val jsons = IListMap("a" -> "123", "b" -> "null", "c" -> "\"str\"", "d" -> "4.5", "e" -> "[1,2,3]", "f" -> "{\"a\": 123, \"b\": 3.14}")
    val sb = new JStringBuilder
    val output = new JsonStringOutput(sb)
    val oo = output.writeObject()
    jsons.foreach { case (fn, fv) => oo.writeField(fn).writeRawJson(fv) }
    oo.finish()
    val jsonList = sb.toString

    assert(jsonList == jsons.map({ case (k, v) => s""""$k":$v""" }).mkString("{", ",", "}"))

    val input = new JsonStringInput(new JsonReader(jsonList))
    val oi = input.readObject()
    val resBuilder = IListMap.newBuilder[String, String]
    while (oi.hasNext) {
      val fi = oi.nextField()
      resBuilder += ((fi.fieldName, fi.readRawJson()))
    }
    assert(resBuilder.result() == jsons)
  }
}
