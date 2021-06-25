package com.avsystem.commons
package misc

import org.scalatest.funsuite.AnyFunSuite

class GraphUtilTest extends AnyFunSuite {
  def edges(node: Int): List[Int] = node match {
    case 1 => List(2)
    case 2 => List(3, 4)
    case 3 => List(5, 6)
    case 4 => List(5)
    case 5 => List()
    case 6 => List(7)
    case 7 => List(2)
  }

  test("dfs") {
    val log = new StringBuilder

    GraphUtils.dfs(1 to 7)(
      edges,
      onEnter = (n, _) => log.append(s"onEnter $n\n"),
      onExit = (n, _) => log.append(s"onExit $n\n"),
      onRevisit = (n, _) => log.append(s"onRevisit $n\n"),
      onCycle = (n, _) => log.append(s"onCycle $n\n"),
    )

    val exp =


      assert(log.result() ==
        """onEnter 1
          |onEnter 2
          |onEnter 3
          |onEnter 5
          |onExit 5
          |onEnter 6
          |onEnter 7
          |onCycle 2
          |onExit 7
          |onExit 6
          |onExit 3
          |onEnter 4
          |onRevisit 5
          |onExit 4
          |onExit 2
          |onExit 1
          |""".stripMargin)
  }
}
