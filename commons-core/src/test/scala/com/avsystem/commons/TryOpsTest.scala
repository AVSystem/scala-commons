package com.avsystem.commons

import com.avsystem.commons.SharedExtensions.TryOps
import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success, Try}

/**
  * @author Wojciech Milewski
  */
class TryOpsTest extends FlatSpec with Matchers {

  "TryOps.sequence" should "convert empty list" in {
    val list: List[Try[Int]] = Nil

    val result = TryOps.sequence(list)

    result.get shouldBe empty
  }

  it should "convert non-empty list" in {
    val list: List[Try[Int]] = Success(1) :: Success(2) :: Success(3) :: Success(4) :: Nil

    val result = TryOps.sequence(list)

    result.get should contain inOrderOnly(1, 2, 3, 4)
  }

  it should "find all exceptions" in {
    val npe = new NullPointerException
    val ise = new IllegalStateException

    val list: List[Try[Int]] = Success(1) :: Success(2) :: Failure(npe) :: Success(3) :: Failure(ise) :: Success(4) :: Nil

    val result = TryOps.sequence(list)

    val exception = the [NullPointerException] thrownBy result.get
    exception shouldBe npe
    exception.getSuppressed should contain (ise)
  }

  "TryOps.traverse" should "convert empty list" in {
    val list: List[Int] = Nil

    val result = TryOps.traverse(list)(Success(_))

    result.get shouldBe empty
  }

  it should "convert non-empty list" in {
    val list: List[Int] = 1 :: 2 :: 3 :: 4 :: Nil

    val result = TryOps.traverse(list)(Success(_))

    result.get should contain inOrderOnly(1, 2, 3, 4)
  }

  it should "find all exceptions" in {
    val npe = new NullPointerException
    val ise = new IllegalStateException

    val list: List[Int] = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil

    val result = TryOps.traverse(list) { i =>
      if (i == 3) Failure(npe)
      else if (i == 5) Failure(ise)
      else Success(i)
    }

    val exception = the [NullPointerException] thrownBy result.get
    exception shouldBe npe
    exception.getSuppressed should contain (ise)
  }

}
