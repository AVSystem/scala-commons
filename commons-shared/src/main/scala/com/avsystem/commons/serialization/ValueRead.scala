package com.avsystem.commons
package serialization

import com.avsystem.commons.serialization.GenCodec.ReadFailure

import scala.util.{Failure, Success, Try}

/**
  * This is pretty much the same thing as `Try` but more lightweight -
  * it simply holds an error message instead of a whole exception with stack trace etc.
  */
sealed trait ValueRead[+A] {
  def isSuccess: Boolean = this match {
    case ReadSuccessful(_) => true
    case ReadFailed(_) => false
  }

  def isFailure: Boolean = !isSuccess

  def toOption: Option[A] = this match {
    case ReadSuccessful(value) => Some(value)
    case ReadFailed(_) => None
  }

  def toEither: Either[String, A] = this match {
    case ReadSuccessful(value) => Right(value)
    case ReadFailed(reason) => Left(reason)
  }

  def toTry: Try[A] = this match {
    case ReadSuccessful(value) => Success(value)
    case ReadFailed(reason) => Failure(new ReadFailure(reason))
  }

  def getOrElse[B >: A](default: String => B): B = this match {
    case ReadSuccessful(value) => value
    case ReadFailed(reason) => default(reason)
  }

  def orElse[B >: A](other: => ValueRead[B]): ValueRead[B] =
    if (isSuccess) this else other

  def get: A =
    getOrElse(reason => throw new ReadFailure(reason))

  def map[B](f: A => B): ValueRead[B] = this match {
    case ReadSuccessful(value) => ReadSuccessful(f(value))
    case rf: ReadFailed => rf
  }

  def flatMap[B](f: A => ValueRead[B]): ValueRead[B] = this match {
    case ReadSuccessful(value) => f(value)
    case rf: ReadFailed => rf
  }

  def flatten[B](implicit ev: A <:< ValueRead[B]): ValueRead[B] =
    flatMap(ev)

  def foreach(f: A => Any): Unit = this match {
    case ReadSuccessful(value) => f(value)
    case ReadFailed(_) =>
  }
}
final case class ReadSuccessful[+A](value: A) extends ValueRead[A]
final case class ReadFailed(reason: String) extends ValueRead[Nothing]