package com.avsystem.commons
package core

import org.openjdk.jmh.annotations._

import scala.annotation.tailrec

case class NullList(value: Int, tail: NullList) {
  @tailrec final def tailrecSum(acc: Int = 0): Int = {
    val newAcc = acc + value
    if (tail == null) newAcc else tail.tailrecSum(newAcc)
  }
}
object NullList {
  final val Example: NullList = (0 until 1000).foldRight(NullList(1000, null)) {
    (value, tail) => NullList(value, tail)
  }
}

case class OptList(value: Int, tail: Opt[OptList]) {
  @tailrec final def tailrecSum(acc: Int = 0): Int = {
    val newAcc = acc + value
    tail match {
      case Opt(t) => t.tailrecSum(newAcc)
      case Opt.Empty => newAcc
    }
  }
}
object OptList {
  final val Example = (0 until 1000).foldRight(OptList(1000, Opt.Empty)) {
    (value, tail) => OptList(value, Opt(tail))
  }
}

case class OptRefList(value: Int, tail: OptRef[OptRefList]) {
  @tailrec final def tailrecSum(acc: Int = 0): Int = {
    val newAcc = acc + value
    tail match {
      case OptRef(t) => t.tailrecSum(newAcc)
      case OptRef.Empty => newAcc
    }
  }
}
object OptRefList {
  final val Example = (0 until 1000).foldRight(OptRefList(1000, OptRef.Empty)) {
    (value, tail) => OptRefList(value, OptRef(tail))
  }
}

case class OptionList(value: Int, tail: Option[OptionList]) {
  @tailrec final def tailrecSum(acc: Int = 0): Int = {
    val newAcc = acc + value
    tail match {
      case Some(t) => t.tailrecSum(newAcc)
      case None => newAcc
    }
  }
}
object OptionList {
  final val Example: OptionList = (0 until 1000).foldRight(OptionList(1000, None)) {
    (value, tail) => OptionList(value, Some(tail))
  }
}

@State(Scope.Thread)
@Warmup(iterations = 5, time = 1)
@Measurement(iterations = 10, time = 2)
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
class OptBenchmarks {

  @Benchmark
  def nullListSum: Int = NullList.Example.tailrecSum()

  @Benchmark
  def optListSum: Int = OptList.Example.tailrecSum()

  @Benchmark
  def optRefListSum: Int = OptRefList.Example.tailrecSum()

  @Benchmark
  def optionListSum: Int = OptionList.Example.tailrecSum()
}
