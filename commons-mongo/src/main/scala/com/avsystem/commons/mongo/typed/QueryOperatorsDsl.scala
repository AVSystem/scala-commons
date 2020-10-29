package com.avsystem.commons
package mongo.typed

import org.bson.{BsonType, BsonValue}

import scala.util.matching.{Regex => SRegex}

trait VanillaQueryOperatorsDsl[T, R] {

  import MongoQueryOperator._

  def format: MongoFormat[T]

  protected def wrapQueryOperator(operator: MongoQueryOperator[T]): R

  def is(value: T): R = wrapQueryOperator(Eq(value, format))
  def isNot(value: T): R = wrapQueryOperator(Ne(value, format))
  def gt(value: T): R = wrapQueryOperator(Gt(value, format))
  def gte(value: T): R = wrapQueryOperator(Gte(value, format))
  def lt(value: T): R = wrapQueryOperator(Lt(value, format))
  def lte(value: T): R = wrapQueryOperator(Lte(value, format))
  def in(values: Iterable[T]): R = wrapQueryOperator(In(values, format))
  def in(values: T*): R = in(values)
  def nin(values: Iterable[T]): R = wrapQueryOperator(Nin(values, format))
  def nin(values: T*): R = nin(values)
  def exists: R = exists(true)
  def exists(exists: Boolean): R = wrapQueryOperator(Exists(exists))
  def hasType(bsonType: BsonType): R = wrapQueryOperator(Type(bsonType))
  def regex(pattern: SRegex): R = wrapQueryOperator(Regex(pattern))
  def regex(pattern: String): R = regex(new SRegex(pattern))

  def text(
    search: String,
    language: OptArg[String] = OptArg.Empty,
    caseSensitive: OptArg[Boolean] = OptArg.Empty,
    diacriticSensitive: OptArg[Boolean] = OptArg.Empty,
  ): R =
    wrapQueryOperator(Text(search, language.toOpt, caseSensitive.toOpt, diacriticSensitive.toOpt))

  def not(filter: MongoFilter.Creator[T] => MongoOperatorsFilter[T]): R =
    wrapQueryOperator(Not(filter(new MongoFilter.Creator(format))))

  def raw(rawOperator: String, bson: BsonValue): R =
    wrapQueryOperator(Raw(rawOperator, bson))
}
object VanillaQueryOperatorsDsl {
  implicit class ForCollection[C[X] <: Iterable[X], T, R](private val dsl: VanillaQueryOperatorsDsl[C[T], R]) extends AnyVal {

    import MongoQueryOperator._

    private def format: MongoFormat[T] = dsl.format.assumeCollection.elementFormat

    def size(size: Int): R = dsl.wrapQueryOperator(Size(size))

    def elemMatch(filter: MongoFilter.Creator[T] => MongoFilter[T]): R =
      dsl.wrapQueryOperator(ElemMatch(filter(new MongoFilter.Creator(format))))

    def all(values: T*): R = all(values)
    def all(values: Iterable[T]): R = dsl.wrapQueryOperator(All(values, format))
  }
}

trait QueryOperatorsDsl[T, R] extends VanillaQueryOperatorsDsl[T, R] {
  def ===(value: T): R = is(value)
  def !==(value: T): R = isNot(value)

  def >(value: T): R = gt(value)
  def >=(value: T): R = gte(value)
  def <(value: T): R = lt(value)
  def <=(value: T): R = lte(value)

  def startsWith(prefix: String): R =
    regex(new SRegex("^" + SRegex.quote(prefix)))

  def containsSubstring(infix: String): R =
    regex(SRegex.quote(infix))
}
object QueryOperatorsDsl {
  implicit class ForCollection[C[X] <: Iterable[X], T, R](private val dsl: QueryOperatorsDsl[C[T], R]) extends AnyVal {
    def isEmpty: R = dsl.size(0)
    def contains(value: T): R = dsl.elemMatch(_.is(value))
    def containsAny(values: T*): R = containsAny(values)
    def containsAny(values: Iterable[T]): R = dsl.elemMatch(_.in(values))
    def containsAll(values: T*): R = containsAll(values)
    def containsAll(values: Iterable[T]): R = dsl.all(values)
  }
}
