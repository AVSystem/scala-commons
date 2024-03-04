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

class JvmTaskExtensionsTest extends AnyFunSuite with Matchers with ScalaCheckDrivenPropertyChecks with ScalaFutures {

  import com.avsystem.commons.concurrent.TaskExtensions._

  private implicit val scheduler: Scheduler = Scheduler.global

  // This test does not work in SJS runtime (but the method itself does)
  test("lazyTimeout") {
    val result = Task.never.lazyTimeout(50.millis, "Lazy timeout").runToFuture.failed.futureValue
    result shouldBe a[TimeoutException]
    result.getMessage shouldBe "Lazy timeout"
  }
}
