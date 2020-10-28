package com.avsystem.commons
package mongo.model

import org.bson.{BsonDocument, BsonRegularExpression, BsonType, BsonValue}

import scala.util.matching.{Regex => SRegex}

sealed trait MongoQueryOperator[T] extends Product {

  import MongoQueryOperator._

  def rawOperator: String = "$" + productPrefix.uncapitalize

  def toFilter: MongoOperatorsFilter[T] =
    MongoOperatorsFilter(Seq(this))

  def toBson: BsonValue = this match {
    case Eq(value, format) => format.writeBson(value)
    case Ne(value, format) => format.writeBson(value)
    case Gt(value, format) => format.writeBson(value)
    case Gte(value, format) => format.writeBson(value)
    case Lt(value, format) => format.writeBson(value)
    case Lte(value, format) => format.writeBson(value)
    case In(values, format) => Bson.array(values.iterator.map(format.writeBson))
    case Nin(values, format) => Bson.array(values.iterator.map(format.writeBson))
    case Exists(exists) => Bson.boolean(exists)
    case Type(bsonType) => Bson.int(bsonType.getValue)
    case Regex(pattern) => new BsonRegularExpression(pattern.toString)
    case Mod(divisor, remainder) => Bson.array(Bson.long(divisor), Bson.long(remainder))
    case Text(search, language, caseSensitive, diacriticSensitive) =>
      val doc = new BsonDocument("$search", Bson.string(search))
      language.foreach(v => doc.put("$language", Bson.string(v)))
      caseSensitive.foreach(v => doc.put("$caseSensitive", Bson.boolean(v)))
      diacriticSensitive.foreach(v => doc.put("$diacriticSensitive", Bson.boolean(v)))
      doc
    case f: Size[_, _] => Bson.int(f.size)
    case f: ElemMatch[_, _] => f.filter.toBson
    case f: All[_, t] => Bson.array(f.values.iterator.map(f.format.writeBson))
    case Not(filter) => filter.toOperatorsBson
    case Raw(_, bson) => bson
  }
}

object MongoQueryOperator {
  implicit def creatorForCollection[C[X] <: Iterable[X], T](creator: Creator[C[T]]): CreatorForCollection[C, T] =
    new CreatorForCollection(creator.format.assumeCollection.elementFormat)

  final class Creator[T](private[MongoQueryOperator] val format: MongoFormat[T]) {
    def is(value: T): MongoQueryOperator[T] = Eq(value, format)
    def isNot(value: T): MongoQueryOperator[T] = Ne(value, format)
    def gt(value: T): MongoQueryOperator[T] = Gt(value, format)
    def gte(value: T): MongoQueryOperator[T] = Gte(value, format)
    def lt(value: T): MongoQueryOperator[T] = Lt(value, format)
    def lte(value: T): MongoQueryOperator[T] = Lte(value, format)
    def in(values: Iterable[T]): MongoQueryOperator[T] = In(values, format)
    def in(values: T*): MongoQueryOperator[T] = in(values)
    def nin(values: Iterable[T]): MongoQueryOperator[T] = Nin(values, format)
    def nin(values: T*): MongoQueryOperator[T] = nin(values)
    def exists: MongoQueryOperator[T] = exists(true)
    def exists(exists: Boolean): MongoQueryOperator[T] = Exists(exists)
    def hasType(bsonType: BsonType): MongoQueryOperator[T] = Type(bsonType)
    def regex(pattern: SRegex): MongoQueryOperator[T] = Regex(pattern)
    def regex(pattern: String): MongoQueryOperator[T] = regex(new SRegex(pattern))

    def text(
      search: String,
      language: OptArg[String] = OptArg.Empty,
      caseSensitive: OptArg[Boolean] = OptArg.Empty,
      diacriticSensitive: OptArg[Boolean] = OptArg.Empty,
    ): MongoQueryOperator[T] =
      Text(search, language.toOpt, caseSensitive.toOpt, diacriticSensitive.toOpt)

    def not(filter: MongoFilter.Creator[T] => MongoOperatorsFilter[T]): MongoQueryOperator[T] = Not(filter(new MongoFilter.Creator(format)))
    def raw(rawOperator: String, bson: BsonValue): MongoQueryOperator[T] = Raw(rawOperator, bson)
  }

  final class CreatorForCollection[C[X] <: Iterable[X], T](format: MongoFormat[T]) {
    def size(size: Int): MongoQueryOperator[C[T]] = Size(size)

    def elemMatch(filter: MongoFilter.Creator[T] => MongoFilter[T]): MongoQueryOperator[C[T]] =
      ElemMatch(filter(new MongoFilter.Creator(format)))

    def all(values: T*): MongoQueryOperator[C[T]] = all(values)
    def all(values: Iterable[T]): MongoQueryOperator[C[T]] = All(values, format)
  }

  final case class Eq[T](value: T, format: MongoFormat[T]) extends MongoQueryOperator[T]
  final case class Ne[T](value: T, format: MongoFormat[T]) extends MongoQueryOperator[T]
  final case class Gt[T](value: T, format: MongoFormat[T]) extends MongoQueryOperator[T]
  final case class Gte[T](value: T, format: MongoFormat[T]) extends MongoQueryOperator[T]
  final case class Lt[T](value: T, format: MongoFormat[T]) extends MongoQueryOperator[T]
  final case class Lte[T](value: T, format: MongoFormat[T]) extends MongoQueryOperator[T]
  final case class In[T](values: Iterable[T], format: MongoFormat[T]) extends MongoQueryOperator[T]
  final case class Nin[T](values: Iterable[T], format: MongoFormat[T]) extends MongoQueryOperator[T]
  final case class Exists[T](exists: Boolean) extends MongoQueryOperator[T]
  final case class Type[T](bsonType: BsonType) extends MongoQueryOperator[T]
  final case class Regex[T](pattern: SRegex) extends MongoQueryOperator[T]
  final case class Mod[T](divisor: Long, remainder: Long) extends MongoQueryOperator[T]

  final case class Text[T](
    search: String,
    language: Opt[String],
    caseSensitive: Opt[Boolean],
    diacriticSensitive: Opt[Boolean]
  ) extends MongoQueryOperator[T]

  final case class Size[C[X] <: Iterable[X], T](size: Int) extends MongoQueryOperator[C[T]]
  final case class ElemMatch[C[X] <: Iterable[X], T](filter: MongoFilter[T]) extends MongoQueryOperator[C[T]]
  final case class All[C[X] <: Iterable[X], T](values: Iterable[T], format: MongoFormat[T]) extends MongoQueryOperator[C[T]]

  final case class Not[T](filter: MongoOperatorsFilter[T]) extends MongoQueryOperator[T]
  final case class Raw[T](override val rawOperator: String, bson: BsonValue) extends MongoQueryOperator[T]
}
