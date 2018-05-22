package com.avsystem.commons
package macros.rpc

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

class ParamsParser[A](params: Seq[A]) {
  private val list = new java.util.LinkedList[A]
  list.addAll(params.asJava)

  def remaining: Seq[A] = list.asScala

  def extractSingle[B](filter: A => Boolean, matcher: A => Res[B], whenAbsent: => Fail, consume: Boolean): Res[B] = {
    val it = list.listIterator()
    def loop(): Res[B] =
      if (it.hasNext) {
        val n = it.next()
        if (filter(n)) {
          if (consume) {
            it.remove()
          }
          matcher(n)
        } else loop()
      } else whenAbsent
    loop()
  }

  def extractOptional[B](filter: A => Boolean, matcher: A => Option[B], consume: Boolean): Option[B] = {
    val it = list.listIterator()
    def loop(): Option[B] =
      if (it.hasNext) {
        val n = it.next()
        if (filter(n)) {
          val res = matcher(n)
          if (consume) {
            res.foreach(_ => it.remove())
          }
          res
        } else loop()
      } else None
    loop()
  }

  def extractMulti[B](filter: A => Boolean, matcher: A => Res[B], consume: Boolean): Res[List[B]] = {
    val it = list.listIterator()
    def loop(result: ListBuffer[B]): Res[List[B]] =
      if (it.hasNext) {
        val n = it.next()
        if (filter(n)) {
          if (consume) {
            it.remove()
          }
          matcher(n) match {
            case Ok(b) =>
              result += b
              loop(result)
            case fail: Fail =>
              fail
          }
        } else loop(result)
      } else Ok(result.result())
    loop(new ListBuffer[B])
  }
}
