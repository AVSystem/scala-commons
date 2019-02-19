package com.avsystem.commons
package rest.openapi

import com.avsystem.commons.rest.RestDataCompanion
import com.avsystem.commons.rest.openapi.adjusters.description
import com.avsystem.commons.serialization.json.JsonStringOutput
import com.avsystem.commons.serialization.{GenCodec, name, transparent}
import org.scalatest.FunSuite

class Fuu[T](thing: T)

class RestSchemaTest extends FunSuite {
  private def schemaStr[T](implicit schema: RestSchema[T]): String =
    JsonStringOutput.writePretty(new InliningResolver().resolve(schema))

  trait Dependency
  object Dependency {
    implicit val codec: GenCodec[Dependency] = null
    implicit val schema: RestSchema[Dependency] = RestSchema.ref("Dependency.json")
  }

  @description("kejs klass")
  case class KejsKlass(
    @name("integer") @customWa(42) int: Int,
    @description("serious dependency") dep: Dependency,
    @description("serious string") str: Opt[String] = Opt.Empty
  )
  object KejsKlass extends RestDataCompanion[KejsKlass]

  test("case class") {
    assert(schemaStr[KejsKlass] ==
      """{
        |  "type": "object",
        |  "description": "kejs klass",
        |  "properties": {
        |    "integer": {
        |      "type": "integer",
        |      "format": "int32",
        |      "default": 42
        |    },
        |    "dep": {
        |      "description": "serious dependency",
        |      "allOf": [
        |        {
        |          "$ref": "Dependency.json"
        |        }
        |      ]
        |    },
        |    "str": {
        |      "type": "string",
        |      "description": "serious string",
        |      "nullable": true,
        |      "default": null
        |    }
        |  },
        |  "required": [
        |    "dep"
        |  ]
        |}""".stripMargin)
  }

  @description("wrapped string")
  @transparent case class Wrap(str: String)
  object Wrap extends RestDataCompanion[Wrap]

  test("transparent wrapper") {
    assert(schemaStr[Wrap] ==
      """{
        |  "type": "string",
        |  "description": "wrapped string"
        |}""".stripMargin)
  }

  case class GenCC[+T >: Null](@customWa[T](null) value: T)
  object GenCC extends RestDataCompanion[GenCC[String]]

  test("generic case class") {
    assert(schemaStr[GenCC[String]] ==
      """{
        |  "type": "object",
        |  "properties": {
        |    "value": {
        |      "type": "string",
        |      "default": null
        |    }
        |  }
        |}""".stripMargin
    )
  }

  sealed trait Opaque
  object OpaqueAU {
    def apply(int: Int, str: String): Opaque = null
    def unapply(opaque: Opaque): Opt[(Int, String)] = Opt.Empty
  }

  implicit val opaqueSchema: RestSchema[Opaque] =
    RestStructure.fromApplyUnapplyProvider[Opaque](OpaqueAU).standaloneSchema

  test("third party type") {
    assert(schemaStr[Opaque] ==
      """{
        |  "type": "object",
        |  "properties": {
        |    "int": {
        |      "type": "integer",
        |      "format": "int32"
        |    },
        |    "str": {
        |      "type": "string"
        |    }
        |  },
        |  "required": [
        |    "int",
        |    "str"
        |  ]
        |}""".stripMargin
    )
  }
}
