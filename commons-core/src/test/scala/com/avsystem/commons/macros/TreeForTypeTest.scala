package com.avsystem.commons
package macros

import scala.language.experimental.macros
import scala.language.higherKinds

object TreeForTypeTest {
  def testTreeForType(tpeRepr: String): Nothing = macro com.avsystem.commons.macros.TestMacros.testTreeForType

  val x = "x"

  type A

  testTreeForType("Int")
  testTreeForType("Integer")
  testTreeForType("A")
  testTreeForType("TreeForTypeTest#B")
  testTreeForType("x.type")
  testTreeForType("None.type")
  testTreeForType("this.type")
  testTreeForType("List[Int]")
  testTreeForType("Set[_]")
  testTreeForType("Map[T, T] forSome {type T <: String}")
  testTreeForType("AnyRef with Serializable")
  testTreeForType(
    """AnyRef {
    type Lol[K, V] <: Map[K, V]
    def stuff[C[+X] <: Traversable[X]](c: C[Int]): C[String]
    val lulz: Map[String, Int]
  }""")
}

/**
  * Author: ghik
  * Created: 25/11/15.
  */
class TreeForTypeTest {

  import TreeForTypeTest._

  type B
  val y = "y"

  class Fuu {
    val z = "z"
    object bar {
      val q = "q"
    }
  }
  val fuu = new Fuu

  testTreeForType("Int")
  testTreeForType("Integer")
  testTreeForType("A")
  testTreeForType("TreeForTypeTest#B")
  testTreeForType("B")
  testTreeForType("x.type")
  testTreeForType("y.type")
  testTreeForType("fuu.bar.q.type")
  testTreeForType("None.type")
  testTreeForType("this.type")
  testTreeForType("List[Int]")
  testTreeForType("Set[_]")
  testTreeForType("Map[T, T] forSome {type T <: String}")
  testTreeForType("fu.z.type forSome {val fu: Fuu}")
  testTreeForType("fu.z.type forSome {val fu: Fuu with Singleton}")
  testTreeForType("fu.bar.q.type forSome {val fu: Fuu}")
  testTreeForType("AnyRef with Serializable")
  testTreeForType(
    """AnyRef {
    type Lol[K, V] <: Map[K, V]
    type Stuff[K, V] = Map[K, V]
    def stuff[C[+X] <: Traversable[X]](c: C[Int]): C[String]
    val lulz: Map[String, Int]
  }""")

  class Inner {
    testTreeForType("Int")
    testTreeForType("Integer")
    testTreeForType("A")
    testTreeForType("TreeForTypeTest#B")
    testTreeForType("B")
    testTreeForType("x.type")
    testTreeForType("y.type")
    testTreeForType("fuu.z.type")
    testTreeForType("fuu.bar.q.type")
    testTreeForType("None.type")
    testTreeForType("this.type")
    testTreeForType("Inner.this.type")
    testTreeForType("TreeForTypeTest.this.type")
    testTreeForType("List[Int]")
    testTreeForType("Set[_]")
    testTreeForType("Map[T, T] forSome {type T <: String}")
    testTreeForType("fu.z.type forSome {val fu: Fuu}")
    testTreeForType("fu.bar.q.type forSome {val fu: Fuu}")
    testTreeForType("AnyRef with Serializable")
    testTreeForType(
      """AnyRef {
      type Lol[K, V] <: Map[K, V]
      type Stuff[K, V] = Map[K, V]
      def stuff[C[+X] <: Traversable[X]](c: C[Int]): C[String]
      val lulz: Map[String, Int]
    }""")
  }
}

object Unrelated {

  import TreeForTypeTest._

  testTreeForType("Int")
  testTreeForType("Integer")
  testTreeForType("A")
  testTreeForType("TreeForTypeTest#B")
  testTreeForType("x.type")
  testTreeForType("None.type")
  testTreeForType("this.type")
  testTreeForType("List[Int]")
  testTreeForType("Set[_]")
  testTreeForType("Map[T, T] forSome {type T <: String}")
  testTreeForType("AnyRef with Serializable")
  testTreeForType(
    """AnyRef {
    type Lol[K, V] <: Map[K, V]
    type Stuff[K, V] = Map[K, V]
    def stuff[C[+X] <: Traversable[X]](c: C[Int]): C[String]
    val lulz: Map[String, Int]
  }""")
}
