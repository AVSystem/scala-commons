package com.avsystem.commons
package misc

import scala.annotation.tailrec

object GraphUtils {
  case class DfsPtr[+T](node: T, edgesLeft: List[T])

  trait DfsAction[-T] {
    def apply(node: T, dfsStack: List[DfsPtr[T]]): Unit
  }

  object NoAction extends DfsAction[Any] {
    def apply(node: Any, dfsStack: List[DfsPtr[Any]]): Unit = ()
  }

  def dfs[T](nodes: Iterable[T])(
    edges: T => List[T],
    onEnter: DfsAction[T] = NoAction,
    onExit: DfsAction[T] = NoAction,
    onRevisit: DfsAction[T] = NoAction,
    onCycle: DfsAction[T] = NoAction
  ): Unit = {
    val visited = new MHashMap[T, Boolean]

    @tailrec
    def loop(stack: List[DfsPtr[T]]): Unit = stack match {
      case DfsPtr(node, deps) :: stackTail => deps match {
        case Nil =>
          onExit(node, stack)
          visited(node) = false
          loop(stackTail)
        case nextDep :: depsTail => visited.get(nextDep) match {
          case None =>
            onEnter(nextDep, stack)
            visited(nextDep) = true
            loop(DfsPtr(nextDep, edges(nextDep)) :: DfsPtr(node, depsTail) :: stackTail)
          case Some(beingVisited) =>
            if (beingVisited) {
              onCycle(nextDep, stack)
            } else {
              onRevisit(nextDep, stack)
            }
            loop(DfsPtr(node, depsTail) :: stackTail)
        }
      }
      case Nil =>
    }

    nodes.foreach { node =>
      if (!visited.contains(node)) {
        val initStack = List(DfsPtr(node, edges(node)))
        onEnter(node, initStack)
        visited(node) = true
        loop(initStack)
      }
    }
  }
}
