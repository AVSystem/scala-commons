package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.mongo.text.TextSearchLanguage
import org.bson.{BsonDocument, BsonType, BsonValue}

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
    case Regex(pattern) => Bson.string(pattern)
    case Mod(divisor, remainder) => Bson.array(Bson.long(divisor), Bson.long(remainder))
    case Text(search, language, caseSensitive, diacriticSensitive) =>
      val doc = new BsonDocument("$search", Bson.string(search))
      language.foreach(v => doc.put("$language", Bson.string(v.code)))
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
  def creator[T: MongoFormat]: Creator[T] = new Creator(MongoFormat[T])

  final class Creator[T](val format: MongoFormat[T])
    extends VanillaQueryOperatorsDsl[T, MongoQueryOperator[T]] {

    protected def wrapQueryOperator(operator: MongoQueryOperator[T]): MongoQueryOperator[T] = operator
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
  final case class Regex[T](pattern: String) extends MongoQueryOperator[T] //TODO: options
  final case class Mod[T](divisor: Long, remainder: Long) extends MongoQueryOperator[T]

  final case class Text[T](
    search: String,
    language: Opt[TextSearchLanguage],
    caseSensitive: Opt[Boolean],
    diacriticSensitive: Opt[Boolean]
  ) extends MongoQueryOperator[T]

  final case class Size[C[X] <: Iterable[X], T](size: Int) extends MongoQueryOperator[C[T]]
  final case class ElemMatch[C[X] <: Iterable[X], T](filter: MongoFilter[T]) extends MongoQueryOperator[C[T]]
  final case class All[C[X] <: Iterable[X], T](values: Iterable[T], format: MongoFormat[T]) extends MongoQueryOperator[C[T]]

  final case class Not[T](filter: MongoOperatorsFilter[T]) extends MongoQueryOperator[T]
  final case class Raw[T](override val rawOperator: String, bson: BsonValue) extends MongoQueryOperator[T]
}
