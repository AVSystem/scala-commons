package com.avsystem.commons
package concurrent

import monix.execution.Scheduler
import monix.reactive.Observable
import org.scalactic.source.Position
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.compat._

class ObservableExtensionsTest extends AnyFunSuite with Matchers
  with ScalaCheckDrivenPropertyChecks with ObservableExtensions with ScalaFutures {
  private implicit val scheduler: Scheduler = Scheduler(RunNowEC)

  test("headOptL") {
    forAll { ints: List[Int] =>
      Observable.fromIterable(ints).headOptL.runToFuture.futureValue shouldBe ints.headOpt
    }
  }
  test("distinct") {
    forAll { ints: List[Int] =>
      Observable.fromIterable(ints).distinct.toListL.runToFuture.futureValue shouldBe ints.distinct
    }
  }
  test("distinctBy") {
    forAll { ints: List[Int] =>
      val f: Int => Int = _ % 256

      Observable.fromIterable(ints).distinctBy(f).toListL.runToFuture.futureValue shouldBe
        ints.foldLeft(MLinkedHashMap.empty[Int, Int])((map, v) => f(v) |> (key => map.applyIf(!_.contains(key))(_ += key -> v))).valuesIterator.toList
    }
  }
  test("sortedL") {
    forAll { ints: List[Int] =>
      Observable.fromIterable(ints).sortedL.runToFuture.futureValue shouldBe ints.sorted
    }
  }
  test("sortedByL") {
    forAll { ints: List[Int] =>
      val f: Int => Int = _ % 256
      Observable.fromIterable(ints).sortedByL(f).runToFuture.futureValue shouldBe ints.sortBy(f)
    }
  }
  test("toL") {
    forAll { ints: List[(Int, Int)] =>
      def testFactory[T](factory: Factory[(Int, Int), T])(implicit position: Position) =
        Observable.fromIterable(ints).toL(factory).runToFuture.futureValue shouldBe factory.fromSpecific(ints)

      testFactory(List)
      testFactory(Vector)
      testFactory(Map)
      testFactory(Set)
      testFactory(Seq)
      testFactory(Vector)
      testFactory(Iterable)
      testFactory(immutable.LazyList)
      testFactory(IHashMap)
      testFactory(IListMap)
      testFactory(ITreeMap)
      testFactory(IHashSet)
      testFactory(ITreeSet)
      testFactory(Array)
      testFactory(MSeq)
      testFactory(MArrayBuffer)
      testFactory(MListBuffer)
      testFactory(MSet)
      testFactory(MHashSet)
      testFactory(MTreeSet)
      testFactory(MLinkedHashSet)
      testFactory(MMap)
      testFactory(MLinkedHashMap)
      testFactory(MHashMap)
      testFactory(MTreeMap)
      testFactory(MSortedSet)
    }
  }

}
