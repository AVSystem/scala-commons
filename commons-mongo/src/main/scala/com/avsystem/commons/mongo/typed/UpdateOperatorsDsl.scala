package com.avsystem.commons
package mongo.typed

trait UpdateOperatorsDsl[T, R] {

  import MongoUpdateOperator._

  def format: MongoFormat[T]
  protected def wrapUpdate(update: MongoUpdate[T]): R

  protected def wrapUpdateOperator(operator: MongoUpdateOperator[T]): R =
    wrapUpdate(MongoUpdate.OperatorUpdate(operator))

  def set(value: T): R = wrapUpdateOperator(Set(value, format))
  def inc(value: T): R = wrapUpdateOperator(Inc(value, format))
  def min(value: T): R = wrapUpdateOperator(Min(value, format))
  def max(value: T): R = wrapUpdateOperator(Max(value, format))
  def mul(value: T): R = wrapUpdateOperator(Mul(value, format))
  def currentDate: R = wrapUpdateOperator(CurrentDate(Opt.Empty))
  def currentDate(tpe: CurrentDateType): R = wrapUpdateOperator(CurrentDate(Opt(tpe)))
  def rename(newPath: String): R = wrapUpdateOperator(Rename(newPath))
  def setOnInsert(value: T): R = wrapUpdateOperator(SetOnInsert(value, format))
  def unset: R = wrapUpdateOperator(Unset())
}
object UpdateOperatorsDsl {
  implicit class ForCollection[C[X] <: Iterable[X], T, R](private val dsl: UpdateOperatorsDsl[C[T], R]) extends AnyVal {

    import MongoUpdateOperator._

    private def format: MongoFormat[T] = dsl.format.assumeCollection.elementFormat

    def push(values: T*): R = push(values)

    def push(
      values: Iterable[T] = Nil,
      position: OptArg[Int] = OptArg.Empty,
      slice: OptArg[Int] = OptArg.Empty,
      sort: OptArg[MongoOrder[T]] = OptArg.Empty
    ): R =
      dsl.wrapUpdateOperator(Push(values, position.toOpt, slice.toOpt, sort.toOpt, format))

    def addToSet(values: T*): R = addToSet(values)
    def addToSet(values: Iterable[T]): R = dsl.wrapUpdateOperator(AddToSet(values, format))
    def popFirst: R = pop(true)
    def popLast: R = pop(false)
    def pop(first: Boolean): R = dsl.wrapUpdateOperator(Pop(first))

    def pull(filter: MongoFilter.Creator[T] => MongoFilter[T]): R =
      dsl.wrapUpdateOperator(Pull(filter(new MongoFilter.Creator(format))))

    def pullAll(values: T*): R = pullAll(values)
    def pullAll(values: Iterable[T]): R = dsl.wrapUpdateOperator(PullAll(values, format))

    def updateFirst(update: MongoUpdate.Creator[T] => MongoUpdate[T]): R = {
      val up = update(new MongoUpdate.Creator(format))
      dsl.wrapUpdate(MongoUpdate.UpdateArrayElements(up, MongoUpdate.ArrayElementsQualifier.First))
    }

    def updateEach(update: MongoUpdate.Creator[T] => MongoUpdate[T]): R = {
      val up = update(new MongoUpdate.Creator(format))
      dsl.wrapUpdate(MongoUpdate.UpdateArrayElements(up, MongoUpdate.ArrayElementsQualifier.Each))
    }
  }
}
