package com.avsystem.commons
package macros.rpc

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

sealed trait Res[+A] {
  def isOk: Boolean = this match {
    case Ok(_) => true
    case _: Fail => false
  }
  def map[B](fun: A => B): Res[B] = this match {
    case Ok(value) => Ok(fun(value))
    case f: Fail => f
  }
  def map2[B, C](other: Res[B])(f: (A, B) => C, ferr: (String, String) => String): Res[C] = (this, other) match {
    case (Ok(a), Ok(b)) => Ok(f(a, b))
    case (Fail(ae), Fail(be)) => Fail(ferr(ae, be))
    case (Ok(_), fail: Fail) => fail
    case (fail: Fail, Ok(_)) => fail
  }
  def flatMap[B](fun: A => Res[B]): Res[B] = this match {
    case Ok(value) => fun(value)
    case f: Fail => f
  }
  def toOption: Option[A] = this match {
    case Ok(value) => Some(value)
    case _ => None
  }
  def foreach(f: A => Any): Unit = this match {
    case Ok(value) => f(value)
    case _ =>
  }
  def mapFailure(f: String => String): Res[A] = this match {
    case Fail(error) => Fail(f(error))
    case _ => this
  }
  def getOrElse[B >: A](forError: String => B): B = this match {
    case Ok(a) => a
    case Fail(error) => forError(error)
  }
}
case class Ok[+T](value: T) extends Res[T]
case class Fail(message: String) extends Res[Nothing]
object Res {
  def traverse[M[X] <: Iterable[X], A, B](in: M[A])(f: A => Res[B])(implicit cbf: CanBuildFrom[M[A], B, M[B]]): Res[M[B]] = {
    val it = in.iterator
    def loop(builder: mutable.Builder[B, M[B]]): Res[M[B]] =
      if (it.hasNext) {
        f(it.next()) match {
          case Ok(b) => loop(builder += b)
          case fail: Fail => fail
        }
      } else Ok(builder.result())
    loop(cbf(in))
  }

  def firstOk[A, B](coll: Iterable[A])(f: A => Res[B])(combineErrors: List[(A, String)] => String): Res[B] = {
    val errors = new ListBuffer[(A, String)]
    def loop(it: Iterator[A]): Res[B] =
      if (it.hasNext) {
        val el = it.next()
        f(el) match {
          case Ok(value) => Ok(value)
          case Fail(error) =>
            errors += ((el, error))
            loop(it)
        }
      } else Fail(combineErrors(errors.result()))
    loop(coll.iterator)
  }
}
