package com.avsystem.commons
package rpc

import com.avsystem.commons.meta.multi
import org.scalatest.funsuite.AnyFunSuite

trait MangleOverloadsRaw {
  @multi
  @mangleOverloads def select(@methodName name: String, @multi args: List[Int]): String
}
object MangleOverloadsRaw extends RawRpcCompanion[MangleOverloadsRaw]

trait Overloads {
  def one: String
  def one(i: Int): String
  def two: String
  def two(i: Int): String
  def two(i: Int, j: Int): String
}
object Overloads {
  implicit val asRawReal: AsRawReal[MangleOverloadsRaw, Overloads] = MangleOverloadsRaw.materializeAsRawReal
}

class MangleOverloadsTest extends AnyFunSuite {
  test("overload mangling") {
    val raw = new MangleOverloadsRaw {
      def select(name: String, args: List[Int]): String = args.mkString(name + "(", ",", ")")
    }
    val real = MangleOverloadsRaw.asReal[Overloads](raw)

    assert(real.one == "one()")
    assert(real.one(42) == "one_1(42)")
    assert(real.two == "two()")
    assert(real.two(42) == "two_1(42)")
    assert(real.two(42, 13) == "two_2(42,13)")
  }
}
