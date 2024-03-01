package com.avsystem.commons
package concurrent

import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.concurrent.TimeoutException
import scala.concurrent.duration._

class TaskExtensionsTest extends AnyFunSuite with Matchers with ScalaCheckDrivenPropertyChecks with ScalaFutures {
  import com.avsystem.commons.concurrent.TaskExtensions._

  private implicit val scheduler: Scheduler = Scheduler.global

  test("lazyTimeout") {
    val result = Task.never.lazyTimeout(50.millis, "Lazy timeout").runToFuture.failed.futureValue
    result shouldBe a[TimeoutException]
    result.getMessage shouldBe "Lazy timeout"
  }

  test("traverseOpt") {
    Task.traverseOpt(Opt.empty[Int])(i => Task.now(i)).runToFuture.futureValue shouldBe Opt.Empty
    Task.traverseOpt(Opt.some(123))(i => Task.now(i)).runToFuture.futureValue shouldBe Opt.some(123)
  }

  test("fromOpt") {
    Task.fromOpt(Opt.empty[Task[Int]]).runToFuture.futureValue shouldBe Opt.Empty
    Task.fromOpt(Opt.some(Task.now(123))).runToFuture.futureValue shouldBe Opt.some(123)
  }

  test("traverseMap") {
    forAll { data: List[(String, Int)] =>
      val map = data.toMap
      val expected = map.view.map({ case (key, value) => (key + key, value + 2) }).toMap
      val result = Task.traverseMap(map)({ case (key, value) => Task((key + key, value + 2)) }).runToFuture.futureValue
      result shouldBe expected
    }
  }

  test("traverseMapValues") {
    forAll { data: List[(String, Int)] =>
      val map = data.toMap
      val expected = map.view.mapValues(value => value + 2).toMap
      val result = Task.traverseMapValues(map)({ case (key, value) => Task(value + 2) }).runToFuture.futureValue
      result shouldBe expected
    }
  }
}
