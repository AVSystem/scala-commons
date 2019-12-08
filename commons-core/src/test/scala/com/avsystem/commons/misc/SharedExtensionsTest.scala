package com.avsystem.commons
package misc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class SharedExtensionsTest extends AnyFunSuite with Matchers {
  test("mkMap") {
    List.range(0, 3).mkMap(identity, _.toString) shouldEqual
      Map(0 -> "0", 1 -> "1", 2 -> "2")
  }

  test("groupToMap") {
    List.range(0, 10).groupToMap(_ % 3, _.toString) shouldEqual
      Map(0 -> List("0", "3", "6", "9"), 1 -> List("1", "4", "7"), 2 -> List("2", "5", "8"))
  }

  test("maxOpt") {
    List.range(0, 10).maxOpt shouldEqual Opt(9)
    List.empty[Int].maxOpt shouldEqual Opt.Empty
  }

  test("Future.eval") {
    val ex = new Exception
    assert(Future.eval(42).value.contains(Success(42)))
    assert(Future.eval(throw ex).value.contains(Failure(ex)))
  }

  test("Future.sequenceCompleted") {
    val p = Promise[Unit]()
    val ex = new RuntimeException
    val failed = Future.failed(ex)

    import com.avsystem.commons.concurrent.RunNowEC.Implicits.executionContext

    val sequence = Future.sequence(Vector(failed, p.future))
    val r1 = Future.sequenceCompleted(Vector(failed, p.future))
    val r2 = Future.sequenceCompleted(Vector(p.future, failed))

    assume(sequence.isCompleted)
    assume(sequence.failed.value.get.get == ex)

    assert(!r1.isCompleted)
    assert(!r2.isCompleted)

    p.success(())

    assert(r1.isCompleted)
    assert(r2.isCompleted)
    assert(r1.value == r2.value)
    assert(r1.value == sequence.value)
  }

  test("Future.traverseCompleted") {
    import com.avsystem.commons.concurrent.RunNowEC.Implicits.executionContext

    val p = Promise[Int]()
    val ex = new RuntimeException
    val in = (1 to 10).toVector

    def fun(i: Int): Future[Int] = i match {
      case 2 => Future.failed(ex)
      case 4 => p.future
      case v => Future.successful(v)
    }

    val traverse = Future.traverse(in)(fun)
    val completed = Future.traverseCompleted(in)(fun)

    assume(traverse.isCompleted)
    assume(traverse.failed.value.get.get == ex)

    assert(!completed.isCompleted)

    p.success(1)

    assert(completed.isCompleted)
    assert(completed.value == traverse.value)
  }

  test("Future.transformWith") {
    import com.avsystem.commons.concurrent.RunNowEC.Implicits._
    val ex = new Exception
    assert(Future.successful(42).transformWith(t => Future.successful(t.get - 1)).value.contains(Success(41)))
    assert(Future.successful(42).transformWith(_ => Future.failed(ex)).value.contains(Failure(ex)))
    assert(Future.failed[Int](ex).transformWith(t => Future.successful(t.failed.get)).value.contains(Success(ex)))
    assert(Future.failed[Int](ex).transformWith(_ => Future.failed(ex)).value.contains(Failure(ex)))
  }

  test("Iterator.untilEmpty") {
    var i = 0
    assert(Iterator.untilEmpty {
      i += 1
      i.opt.filter(_ <= 5)
    }.toList == List(1, 2, 3, 4, 5))
  }

  test("Iterator.iterateUntilEmpty") {
    assert(Iterator.iterateUntilEmpty(Opt.empty[Int])(i => (i + 1).opt).toList == Nil)
    assert(Iterator.iterateUntilEmpty(1.opt)(i => (i + 1).opt.filter(_ <= 5)).toList == List(1, 2, 3, 4, 5))
  }

  test("IteratorOps.collectWhileDefined") {
    assert(Iterator(1, 2, 3, 2, 1).collectWhileDefined({ case n if n < 3 => n * 2 }).toList == List(2, 4))
    assert(Iterator[Int]().collectWhileDefined({ case n if n < 3 => n * 2 }).toList == Nil)
    assert(Iterator(1, 2, 3, 2, 1).collectWhileDefined({ case n if n > 0 => n * 2 }).toList == List(2, 4, 6, 4, 2))
  }

  test("IteratorOps.distinctBy") {
    assert(
      Iterator("ab", "ba", "ac", "cd", "ad", "bd", "be", "fu").distinctBy(_.charAt(0)).toList ==
        List("ab", "ba", "cd", "fu")
    )
  }

  test("uncheckedMatch") {
    val res = Option(42) uncheckedMatch {
      case Some(int) => int
    }
    assert(res == 42)

    assertThrows[MatchError] {
      Option.empty[Int] uncheckedMatch {
        case Some(int) => int
      }
    }
  }

  test("Ordering.orElse") {
    case class CC(f: Int, s: Int)

    val o = Ordering.by((_: CC).f).orElseBy(_.s)

    assert(o.compare(CC(0, 1), CC(1, 0)) < 0)
    assert(o.compare(CC(1, 0), CC(0, 1)) > 0)
    assert(o.compare(CC(0, 0), CC(0, 1)) < 0)
    assert(o.compare(CC(0, 1), CC(0, 0)) > 0)
    assert(o.compare(CC(0, 0), CC(0, 0)) == 0)
  }

  test("String.unwrapLines") {
    assert("".unwrapLines == "")
    assert("\n".unwrapLines == "")
    assert("a\n".unwrapLines == "a")
    assert("\na".unwrapLines == "a")
    assert("\n\n".unwrapLines == "\n")
    assert("a\n\nb".unwrapLines == "a\nb")
    assert("a\nb".unwrapLines == "a b")
    assert("a \nb".unwrapLines == "a b")
    assert("a\n b".unwrapLines == "a b")
    assert("a \n b".unwrapLines == "a  b")
  }

  test("sourceCode") {
    assert(123.sourceCode == "123")

    val src = {
      println(123)
      val x = 5 + 2
    }.sourceCode

    assert(src ==
      """{
        |  println(123)
        |  val x = 5 + 2
        |}""".stripMargin)
  }

  test("withSourceCode") {
    assert(123.withSourceCode == (123, "123"))
  }

  test("flatCollect") {
    val it = Iterator(69, 42)
    val fc = it.flatCollect { case i if i % 2 == 0 => Iterator(-i, i) }
    assert(it.hasNext) //flatCollect should not consume eagerly
    assert(fc.hasNext)
    assert(!it.hasNext)
    fc.toSeq should contain theSameElementsInOrderAs Seq(-42, 42)
  }
}
