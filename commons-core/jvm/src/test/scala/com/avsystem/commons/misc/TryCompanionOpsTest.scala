package com.avsystem.commons
package misc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * @author Wojciech Milewski
  */
class TryCompanionOpsTest extends AnyFlatSpec with Matchers {

  "Try.sequence" should "convert empty list" in {
    val list: List[Try[Int]] = Nil

    val result = Try.sequence(list)

    result.get shouldBe empty
  }

  it should "convert non-empty list" in {
    val list: List[Try[Int]] = Success(1) :: Success(2) :: Success(3) :: Success(4) :: Nil

    val result = Try.sequence(list)

    result.get should contain inOrderOnly(1, 2, 3, 4)
  }

  it should "find all exceptions" in {
    val npe = new NullPointerException
    val ise = new IllegalStateException

    val list: List[Try[Int]] = Success(1) :: Success(2) :: Failure(npe) :: Success(3) :: Failure(ise) :: Success(4) :: Nil

    val result = Try.sequence(list)

    val exception = the [NullPointerException] thrownBy result.get
    exception shouldBe npe
    exception.getSuppressed should contain (ise)
  }

  "Try.traverse" should "convert empty list" in {
    val list: List[Int] = Nil

    val result = Try.traverse(list)(Success(_))

    result.get shouldBe empty
  }

  it should "convert non-empty list" in {
    val list: List[Int] = 1 :: 2 :: 3 :: 4 :: Nil

    val result = Try.traverse(list)(Success(_))

    result.get should contain inOrderOnly(1, 2, 3, 4)
  }

  it should "find all exceptions" in {
    val npe = new NullPointerException
    val ise = new IllegalStateException

    val list: List[Int] = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil

    val result = Try.traverse(list) { i =>
      if (i == 3) Failure(npe)
      else if (i == 5) Failure(ise)
      else Success(i)
    }

    val exception = the [NullPointerException] thrownBy result.get
    exception shouldBe npe
    exception.getSuppressed should contain (ise)
  }

}
