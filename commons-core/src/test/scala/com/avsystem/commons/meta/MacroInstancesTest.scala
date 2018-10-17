package com.avsystem.commons
package meta

import com.avsystem.commons.serialization.GenCodec

case class Dep(int: Int)
case class Klass[T](value: T)

object DependencyImplicits {
  implicit val depCodec: GenCodec[Dep] = GenCodec.materialize
}

trait ComplexInstances[T] {
  def plainCodec: GenCodec[Klass[Int]]
  def codecWithGeneric: GenCodec[Klass[T]]
  def dependencyUsingCodec: GenCodec[Klass[Dep]]
  def parameterizedCodec[A: GenCodec]: GenCodec[Klass[A]]
}

abstract class HasComplexInstances[T](
  implicit macroInstances: MacroInstances[DependencyImplicits.type, ComplexInstances[T]]
) {
  val instances: ComplexInstances[T] = macroInstances(DependencyImplicits, this)
}

object MacroInstancesTest extends HasComplexInstances[String] {
  def main(args: Array[String]): Unit = {
    println(instances.parameterizedCodec[Double])
  }
}
