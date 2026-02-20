package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class ValueEnumExhaustiveMatchTest extends AnyFunSuite with AnalyzerTest {

  // Only enable the valueEnumExhaustiveMatch rule to avoid interference from other rules
  override protected def pluginOptions: List[String] = List("AVSystemAnalyzer:+valueEnumExhaustiveMatch")

  // Minimal compilable ValueEnum stubs for test compilation.
  // The real ValueEnum lives in the core module which is not on the analyzer test classpath.
  private val valueEnumStubs =
    """
      |package com.avsystem.commons.misc
      |
      |trait NamedEnum extends Serializable {
      |  def name: String
      |}
      |
      |sealed trait EnumCtx extends Any {
      |  def ordinal: Int
      |  def valName: String
      |}
      |
      |trait ValueEnum extends NamedEnum {
      |  protected def enumCtx: EnumCtx
      |  def ordinal: Int = enumCtx.ordinal
      |  def name: String = enumCtx.valName
      |}
      |
      |abstract class AbstractValueEnum(
      |  protected implicit val enumCtx: EnumCtx,
      |) extends ValueEnum
      |
      |trait ValueEnumCompanion[T <: ValueEnum] {
      |  type Value = T
      |  private var _ordinal = 0
      |  implicit def ctx: EnumCtx = {
      |    val ord = _ordinal
      |    _ordinal += 1
      |    new EnumCtx {
      |      def ordinal: Int = ord
      |      def valName: String = "val" + ord
      |    }
      |  }
      |}
      |
      |abstract class AbstractValueEnumCompanion[T <: ValueEnum]
      |  extends ValueEnumCompanion[T]
      |""".stripMargin

  private def source(caseDefs: String): String =
    valueEnumStubs + scala"""
                            |import com.avsystem.commons.misc._
                            |
                            |final class Enumz(implicit enumCtx: EnumCtx) extends AbstractValueEnum
                            |object Enumz extends AbstractValueEnumCompanion[Enumz] {
                            |  final val One, Two, Three: Value = new Enumz
                            |}
                            |
                            |object Main {
                            |  val value: Enumz = Enumz.One
                            |  import Enumz._
                            |  value match {
                            |    $caseDefs
                            |  }
                            |}
                            |""".stripMargin

  test("should report two unmatched enum values") {
    assertErrors(
      1,
      source(
        """
          |case Enumz.One =>
          |case null =>
        """.stripMargin,
      ),
    )
  }

  test("should report one unmatched enum value") {
    assertErrors(
      1,
      source(
        """
          |case Enumz.One =>
          |case Enumz.Two =>
        """.stripMargin,
      ),
    )
  }

  test("should report one unmatched by alternative enum value") {
    assertErrors(
      1,
      source(
        """
          |case One | Two =>
        """.stripMargin,
      ),
    )
  }

  test("should not report unmatched values on wildcard") {
    assertNoErrors(
      source(
        """
          |case _ =>
        """.stripMargin,
      ),
    )
  }

  test("should not report unmatched values with guard") {
    assertNoErrors(
      source(
        """
          |case x if x.ordinal > 1 =>
        """.stripMargin,
      ),
    )
  }

  test("should not report no unmatched values in alternative") {
    assertNoErrors(
      source(
        """
          |case One | Two | Three =>
        """.stripMargin,
      ),
    )
  }

  test("should not report no unmatched values") {
    assertNoErrors(
      source(
        """
          |case Enumz.One =>
          |case Enumz.Two =>
          |case Three =>
        """.stripMargin,
      ),
    )
  }
}
