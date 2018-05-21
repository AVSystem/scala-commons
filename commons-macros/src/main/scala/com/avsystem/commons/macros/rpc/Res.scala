package com.avsystem.commons
package macros.rpc

sealed trait Res[+A] {
  def map[B](fun: A => B): Res[B] = this match {
    case Ok(value) => Ok(fun(value))
    case f: Fail => f
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
}
case class Ok[+T](value: T) extends Res[T]
case class Fail(message: String) extends Res[Nothing]