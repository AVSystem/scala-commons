package com.avsystem.commons
package collection

final class MutableStack[T] {
  private var ssize: Int = 0
  private var stack = List.empty[T]

  def push(elem: T): Unit = {
    stack ::= elem
    ssize += 1
  }

  def pushAll(elems: IterableOnce[T]): Unit =
    elems.iterator.foreach(push)

  def pop(): T = stack match {
    case head :: tail =>
      stack = tail
      ssize -= 1
      head
    case Nil => throw new NoSuchElementException("pop on empty stack")
  }

  def popOpt(): Opt[T] = stack match {
    case head :: tail =>
      stack = tail
      ssize -= 1
      Opt.some(head)
    case Nil => Opt.Empty
  }

  def popOption(): Option[T] = stack match {
    case head :: tail =>
      stack = tail
      ssize -= 1
      Some(head)
    case Nil => None
  }

  def top: T = stack match {
    case head :: _ => head
    case _ => throw new NoSuchElementException("top on empty stack")
  }

  def topOpt: Opt[T] = stack match {
    case head :: _ => Opt.some(head)
    case _ => Opt.Empty
  }

  def topOption: Option[T] =
    stack.headOption

  def asList: List[T] =
    stack

  def size: Int =
    ssize

  def isEmpty: Boolean =
    stack.isEmpty
}
