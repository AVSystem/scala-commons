package com.avsystem.commons
package macros.misc

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

sealed trait Res[+A] {
  def isOk: Boolean = this match {
    case Ok(_) => true
    case _: FailMsg => false
    case Fail => false
  }
  def map[B](fun: A => B): Res[B] = this match {
    case Ok(value) => Ok(fun(value))
    case f: FailMsg => f
    case Fail => Fail
  }
  def flatMap[B](fun: A => Res[B]): Res[B] = this match {
    case Ok(value) => fun(value)
    case f: FailMsg => f
    case Fail => Fail
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
    case FailMsg(error) => FailMsg(f(error))
    case _ => this
  }
  def getOrElse[B >: A](forError: Option[String] => B): B = this match {
    case Ok(a) => a
    case FailMsg(error) => forError(Some(error))
    case Fail => forError(None)
  }
}
case class Ok[+T](value: T) extends Res[T]
case class FailMsg(message: String) extends Res[Nothing]
object Fail extends Res[Nothing]
object Res {
  def traverse[A, B](in: List[A])(f: A => Res[B]): Res[List[B]] = {
    val it = in.iterator
    @tailrec def loop(builder: mutable.Builder[B, List[B]]): Res[List[B]] =
      if (it.hasNext) {
        f(it.next()) match {
          case Ok(b) => loop(builder += b)
          case fail: FailMsg => fail
          case Fail => Fail
        }
      } else Ok(builder.result())
    loop(new ListBuffer[B])
  }

  def sequence[A](in: List[Res[A]]): Res[List[A]] =
    traverse(in)(identity)

  def sequence[A](in: Option[Res[A]]): Res[Option[A]] = in match {
    case Some(res) => res.map(Some(_))
    case None => Ok(None)
  }

  def firstOk[A, B](coll: Iterable[A])(f: A => Res[B])(combineErrors: List[(A, String)] => String): Res[B] =
    firstOk(coll.iterator)(f)(combineErrors)

  def firstOk[A, B](it: Iterator[A])(f: A => Res[B])(combineErrors: List[(A, String)] => String): Res[B] = {
    val errors = new ListBuffer[(A, String)]
    @tailrec def loop(it: Iterator[A]): Res[B] =
      if (it.hasNext) {
        val el = it.next()
        f(el) match {
          case Ok(value) => Ok(value)
          case FailMsg(error) =>
            errors += ((el, error))
            loop(it)
          case Fail =>
            loop(it)
        }
      } else FailMsg(combineErrors(errors.result()))
    loop(it)
  }
}
