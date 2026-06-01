package com.avsystem.commons

import com.avsystem.commons.SharedExtensions.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Tests for the `show*` / `sourceCode` / `withSourceCode` debug macros on `UniversalOps[A]`.
  *
  * Compile-time `report.info` output isn't captured here (would need a custom compile harness). We verify:
  *   - **Identity**: each `show*` returns the receiver unchanged at runtime.
  *   - **Type preservation**: static type isn't widened.
  *   - **Generic / parameterised receivers**: macros work across primitives, collections, ADTs, tuples, function
  *     values, generics.
  *   - **Single evaluation**: receiver expression is evaluated exactly once.
  *   - **sourceCode / withSourceCode**: captured source contains the literal text.
  */
class SharedExtensionsShowTest extends AnyFunSuite with Matchers {

  // === show* — identity on primitives ===

  test("showAst preserves Int")(42.showAst shouldEqual 42)
  test("showAst preserves negative Int")((-7).showAst shouldEqual -7)
  test("showAst preserves Int arithmetic result")((2 + 3 * 4).showAst shouldEqual 14)
  test("showAst preserves Long")(1234567890123L.showAst shouldEqual 1234567890123L)
  test("showAst preserves Double")(3.14.showAst shouldEqual 3.14)
  test("showAst preserves Boolean true")(true.showAst shouldEqual true)
  test("showAst preserves Boolean false")(false.showAst shouldEqual false)
  test("showAst preserves Char")('x'.showAst shouldEqual 'x')
  test("showAst preserves Unit")(().showAst shouldEqual (()))

  // === show* — identity on String ===

  test("showAst preserves String")("hello".showAst shouldEqual "hello")
  test("showAst preserves empty String")("".showAst shouldEqual "")
  test("showSymbol preserves String")("abc".showSymbol shouldEqual "abc")

  // === show* — identity on collections ===

  test("showAst preserves List[Int]")(List(1, 2, 3).showAst shouldEqual List(1, 2, 3))
  test("showAst preserves empty List")(List.empty[Int].showAst shouldEqual List.empty[Int])
  test("showAst preserves Map") {
    val m = Map("a" -> 1, "b" -> 2)
    m.showAst shouldEqual m
  }
  test("showAst preserves Set")(Set(1, 2, 3).showAst shouldEqual Set(1, 2, 3))
  test("showType preserves Option[Int] Some")((Some(5): Option[Int]).showType shouldEqual Some(5))
  test("showType preserves Option[Int] None")((None: Option[Int]).showType shouldEqual None)
  test("showRawType preserves Vector")(Vector(1, 2, 3).showRawType shouldEqual Vector(1, 2, 3))
  test("showRawType preserves Range")((1 to 5).showRawType.toList shouldEqual List(1, 2, 3, 4, 5))

  // === show* — identity on tuples / functions / sealed / nested ===

  test("showAst preserves Tuple2")((1, "x").showAst shouldEqual ((1, "x")))
  test("showAst preserves Tuple3") {
    val t = (1, "x", true)
    t.showAst shouldEqual ((1, "x", true))
  }
  test("showAst preserves function value") {
    val f: Int => Int = _ + 1
    f.showAst(10) shouldEqual 11
  }
  test("showAst preserves Either Right") {
    val e: Either[String, Int] = Right(7)
    e.showAst shouldEqual Right(7)
  }
  test("showRawType preserves Either Left") {
    val e: Either[String, Int] = Left("err")
    e.showRawType shouldEqual Left("err")
  }
  test("showTypeSymbol preserves sealed-trait case-object") {
    val s: SealedSample = SealedSample.A
    s.showTypeSymbol shouldEqual SealedSample.A
  }
  test("showTypeSymbol preserves sealed-trait case-class") {
    val s: SealedSample = SealedSample.B(42)
    s.showTypeSymbol shouldEqual SealedSample.B(42)
  }
  test("showAst preserves nested collection") {
    val n: List[Option[Int]] = List(Some(1), None, Some(3))
    n.showAst shouldEqual List(Some(1), None, Some(3))
  }

  // === show* — works inside generic context ===

  def echo[A](a: A): A = a.showAst
  def echoType[A](a: A): A = a.showType
  def echoSym[A](a: A): A = a.showSymbol

  test("showAst on generic type parameter — Int")(echo(123) shouldEqual 123)
  test("showAst on generic type parameter — String")(echo("g") shouldEqual "g")
  test("showAst on generic type parameter — List[Double]") {
    echo(List(1.0, 2.0)) shouldEqual List(1.0, 2.0)
  }
  test("showType on generic type parameter")(echoType(true) shouldEqual true)
  test("showSymbol on generic type parameter")(echoSym(7L) shouldEqual 7L)

  // === show* — static type preservation ===

  test("showAst preserves static type — String") {
    val s: String = "x".showAst
    s shouldEqual "x"
  }
  test("showAst preserves static type — List[Int]") {
    val xs: List[Int] = List(1, 2).showAst
    xs shouldEqual List(1, 2)
  }
  test("showAst preserves static type — Option[String]") {
    val o: Option[String] = Some("h").showAst
    o shouldEqual Some("h")
  }
  test("showType preserves static type") {
    val n: Int = 5.showType
    n shouldEqual 5
  }
  test("showSymbol preserves static type — Option") {
    val o: Option[String] = Some("h").showSymbol
    o shouldEqual Some("h")
  }
  test("showRawType preserves static type — Map") {
    val m: Map[String, Int] = Map("k" -> 1).showRawType
    m shouldEqual Map("k" -> 1)
  }
  test("showTypeSymbol preserves static type — Tuple") {
    val t: (Int, String) = (1, "x").showTypeSymbol
    t shouldEqual ((1, "x"))
  }

  // === show* — receiver evaluated exactly once ===

  test("showAst evaluates receiver exactly once") {
    var count = 0
    def side(): Int = { count += 1; 99 }
    side().showAst shouldEqual 99
    count shouldEqual 1
  }
  test("showType evaluates receiver exactly once") {
    var count = 0
    def side(): String = { count += 1; "s" }
    side().showType shouldEqual "s"
    count shouldEqual 1
  }
  test("showSymbol evaluates receiver exactly once") {
    var count = 0
    def side(): Long = { count += 1; 42L }
    side().showSymbol shouldEqual 42L
    count shouldEqual 1
  }
  test("showTypeSymbol evaluates receiver exactly once") {
    var count = 0
    def side(): Boolean = { count += 1; true }
    side().showTypeSymbol shouldEqual true
    count shouldEqual 1
  }
  test("withSourceCode evaluates receiver exactly once") {
    var count = 0
    def side(): Int = { count += 1; 7 }
    val (v, _) = side().withSourceCode
    v shouldEqual 7
    count shouldEqual 1
  }

  // === show* — chain composition ===

  test("showAst chains with itself") {
    42.showAst.showAst.showAst shouldEqual 42
  }
  test("show* chain across different variants") {
    val v = "hi".showAst.showSymbol.showType.showRawType.showTypeSymbol
    v shouldEqual "hi"
  }
  test("show* chains with debugMacro") {
    val v = 42.showAst.debugMacro
    v shouldEqual 42
  }

  // === sourceCode — capture literal text ===

  test("sourceCode captures arithmetic literal") {
    val src = (1 + 2).sourceCode
    src should include("1 + 2")
  }
  test("sourceCode captures variable name") {
    val x = 10
    val src = x.sourceCode
    src should include("x")
  }
  test("sourceCode captures method-chain") {
    val src = "hello".toUpperCase.sourceCode
    src should include("\"hello\".toUpperCase")
  }
  test("sourceCode captures collection literal") {
    val src = List(1, 2, 3).sourceCode
    src should include("List(1, 2, 3)")
  }
  test("sourceCode captures nested arithmetic") {
    val src = ((1 + 2) * (3 - 4)).sourceCode
    src should (include("1 + 2").and(include("3 - 4")))
  }
  test("sourceCode captures string interpolation expression") {
    val name = "world"
    val src = s"hello $name".sourceCode
    src should include("hello")
  }
  test("sourceCode is non-empty") {
    val src = 1.sourceCode
    src should not be empty
  }

  // === withSourceCode — paired value + source ===

  test("withSourceCode returns matching value and source — arithmetic") {
    val (value, src) = (40 + 2).withSourceCode
    value shouldEqual 42
    src should include("40 + 2")
  }
  test("withSourceCode — String literal") {
    val (value, src) = "world".withSourceCode
    value shouldEqual "world"
    src should include("\"world\"")
  }
  test("withSourceCode — method call") {
    val (size, src) = "abc".length.withSourceCode
    size shouldEqual 3
    src should include("\"abc\".length")
  }
  test("withSourceCode — collection literal") {
    val (xs, src) = List(1, 2, 3).withSourceCode
    xs shouldEqual List(1, 2, 3)
    src should include("List(1, 2, 3)")
  }
  test("withSourceCode preserves receiver type") {
    val (xs, _): (List[Int], String) = List(1, 2, 3).withSourceCode
    xs shouldEqual List(1, 2, 3)
  }
  test("withSourceCode source field non-empty") {
    val (_, src) = 1.withSourceCode
    src should not be empty
  }

  // === Compile-test smoke for non-trivial generic / refined types ===

  test("showType on parameterised function") {
    val f: (Int, String) => Boolean = (_, _) => true
    f.showType(1, "x") shouldEqual true
  }
  test("showRawType on Map of generic") {
    val m: Map[String, List[Int]] = Map("k" -> List(1, 2))
    m.showRawType shouldEqual Map("k" -> List(1, 2))
  }
  test("showSymbol on lambda") {
    val f = (x: Int) => x * 2
    f.showSymbol(3) shouldEqual 6
  }
  test("showTypeSymbol on Option of sealed") {
    val o: Option[SealedSample] = Some(SealedSample.A)
    o.showTypeSymbol shouldEqual Some(SealedSample.A)
  }
}

sealed trait SealedSample
object SealedSample {
  case object A extends SealedSample
  final case class B(value: Int) extends SealedSample
}
