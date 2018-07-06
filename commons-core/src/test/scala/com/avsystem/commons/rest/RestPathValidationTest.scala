package com.avsystem.commons
package rest

import org.scalatest.FunSuite

class RestPathValidationTest extends FunSuite {
  trait Api2 {
    def self: Api2
  }
  object Api2 {
    implicit val metadata: RestMetadata[Api2] = RestMetadata.materializeForRpc[Api2]
  }

  test("recursive API") {
    val failure = intercept[IllegalArgumentException](Api2.metadata.ensureUnambiguousPaths())
    assert(failure.getMessage == "call chain self->self is recursive, recursively defined server APIs are forbidden")
  }

  trait Api1 {
    @GET("p") def g1: Future[String]
    @GET("p") def g2: Future[String]
    @GET("") def g3(@Path("p") arg: String): Future[String]

    @POST("p") def p1: Future[String]
  }

  test("simple ambiguous paths") {
    val failure = intercept[IllegalArgumentException] {
      RestMetadata.materializeForRpc[Api1].ensureUnambiguousPaths()
    }
    assert(failure.getMessage ==
      """REST API has ambiguous paths:
        |GET /p may result from multiple calls:
        |  g2
        |  g1""".stripMargin
    )
  }
}
