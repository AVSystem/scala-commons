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
      sort: OptArg[MongoOrder[T]] = OptArg.Empty,
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

    /** Uses [[https://docs.mongodb.com/manual/reference/operator/update/positional/ the $$ positional operator]] to
      * update first element of an array field that matches the query document. The array field must appear as part of
      * the query document.
      */
    def updateFirstMatching(update: MongoUpdate.Creator[T] => MongoUpdate[T]): R = {
      val up = update(new MongoUpdate.Creator(format))
      dsl.wrapUpdate(MongoUpdate.UpdateArrayElements(up, MongoUpdate.ArrayElementsQualifier.FirstMatching()))
    }

    /** Uses
      * [[https://docs.mongodb.com/manual/reference/operator/update/positional-all/ the all $$[] positional operator]]
      * to update all elements of an array field.
      */
    def updateAll(update: MongoUpdate.Creator[T] => MongoUpdate[T]): R = {
      val up = update(new MongoUpdate.Creator(format))
      dsl.wrapUpdate(MongoUpdate.UpdateArrayElements(up, MongoUpdate.ArrayElementsQualifier.Each()))
    }

    /** Uses the
      * [[https://docs.mongodb.com/manual/reference/operator/update/positional-filtered/ the filtered positional operator]]
      * to update all elements of an array matching a specified filter. The filter is added to
      * [[https://docs.mongodb.com/manual/release-notes/3.6/#arrayfilters arrayFilters]] option of an update operation.
      */
    def updateFiltered(filter: MongoFilter.Creator[T] => MongoFilter[T], update: MongoUpdate.Creator[T] => MongoUpdate[T])
      : R = {
      val fil = filter(new MongoFilter.Creator(format))
      val up = update(new MongoUpdate.Creator(format))
      dsl.wrapUpdate(MongoUpdate.UpdateArrayElements(up, MongoUpdate.ArrayElementsQualifier.Filtered(fil)))
    }
  }
}
