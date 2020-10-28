package com.avsystem.commons
package mongo.model

import com.avsystem.commons.annotation.macroPrivate
import org.bson.{BsonArray, BsonDocument, BsonType}

import scala.util.matching.Regex

sealed trait MongoFilter[T] {
  def on[E](prefix: MongoPropertyRef[E, T]): MongoDocumentFilter[E] =
    MongoDocumentFilter.PropertyValueFilter(prefix, this)

  final def toBson: BsonDocument = this match {
    case docFilter: MongoDocumentFilter[T] => docFilter.toFilterBson(Opt.Empty)
    case opsFilter: MongoOperatorsFilter[T] => opsFilter.toOperatorsBson
  }
}

object MongoFilter {
  def apply[T: MongoFormat]: Creator[T] = new Creator(MongoFormat[T])

  implicit def creatorForCollection[C[X] <: Iterable[X], T](creator: Creator[C[T]]): CreatorForCollection[C, T] =
    new CreatorForCollection(creator.format)

  class Creator[T](private[MongoFilter] val format: MongoFormat[T]) extends AnyVal with HasRefMacros[T, T] {
    type ThisDataRef[C <: T] = MongoDataRef[T, C]

    @macroPrivate def thisDataRef: ThisDataRef[T] = MongoRef.SelfRef(format.assumeAdt)

    private def singleOp(op: MongoQueryOperator.Creator[T] => MongoQueryOperator[T]): MongoOperatorsFilter[T] =
      satisfiesOperators(c => Seq(op(c)))

    def satisfiesOperators(operators: MongoQueryOperator.Creator[T] => Seq[MongoQueryOperator[T]]): MongoOperatorsFilter[T] =
      MongoOperatorsFilter(operators(new MongoQueryOperator.Creator(format)))

    def exists: MongoOperatorsFilter[T] = exists(true)
    def exists(exists: Boolean): MongoOperatorsFilter[T] = singleOp(_.exists(exists))
    def hasType(bsonType: BsonType): MongoOperatorsFilter[T] = singleOp(_.hasType(bsonType))
    def is(value: T): MongoOperatorsFilter[T] = singleOp(_.is(value))
    def isNot(value: T): MongoOperatorsFilter[T] = singleOp(_.isNot(value))
    def in(values: T*): MongoOperatorsFilter[T] = in(values)
    def in(values: Iterable[T]): MongoOperatorsFilter[T] = singleOp(_.in(values))
    def nin(values: T*): MongoOperatorsFilter[T] = nin(values)
    def nin(values: Iterable[T]): MongoOperatorsFilter[T] = singleOp(_.nin(values))
    def gt(value: T): MongoOperatorsFilter[T] = singleOp(_.gt(value))
    def >(value: T): MongoOperatorsFilter[T] = gt(value)
    def gte(value: T): MongoOperatorsFilter[T] = singleOp(_.gte(value))
    def >=(value: T): MongoOperatorsFilter[T] = gte(value)
    def lt(value: T): MongoOperatorsFilter[T] = singleOp(_.lt(value))
    def <(value: T): MongoOperatorsFilter[T] = lt(value)
    def lte(value: T): MongoOperatorsFilter[T] = singleOp(_.lte(value))
    def <=(value: T): MongoOperatorsFilter[T] = lte(value)
    def regex(pattern: Regex): MongoOperatorsFilter[T] = singleOp(_.regex(pattern))
    def regex(pattern: String): MongoOperatorsFilter[T] = singleOp(_.regex(pattern))
    def startsWith(prefix: String): MongoOperatorsFilter[T] = regex(new Regex("^" + Regex.quote(prefix)))
  }

  class CreatorForCollection[C[X] <: Iterable[X], T](private val format: MongoFormat[C[T]]) extends AnyVal {
    def elementFormat: MongoFormat[T] = format.assumeCollection.elementFormat

    private def singleOp(op: MongoQueryOperator.CreatorForCollection[C, T] => MongoQueryOperator[C[T]]): MongoOperatorsFilter[C[T]] =
      MongoOperatorsFilter(Seq(op(new MongoQueryOperator.CreatorForCollection(elementFormat))))

    def size(size: Int): MongoOperatorsFilter[C[T]] = singleOp(_.size(size))
    def isEmpty: MongoOperatorsFilter[C[T]] = singleOp(_.size(0))
    def elemMatch(filter: Creator[T] => MongoFilter[T]): MongoOperatorsFilter[C[T]] = singleOp(_.elemMatch(filter))
    def contains(value: T): MongoOperatorsFilter[C[T]] = elemMatch(_.is(value))
    def containsAny(values: T*): MongoOperatorsFilter[C[T]] = containsAny(values)
    def containsAny(values: Iterable[T]): MongoOperatorsFilter[C[T]] = elemMatch(_.in(values))
    def containsAll(values: T*): MongoOperatorsFilter[C[T]] = containsAll(values)
    def containsAll(values: Iterable[T]): MongoOperatorsFilter[C[T]] = singleOp(_.all(values))
  }
}

final case class MongoOperatorsFilter[T](operators: Seq[MongoQueryOperator[T]]) extends MongoFilter[T] {
  def negated: MongoOperatorsFilter[T] =
    MongoOperatorsFilter(Seq(MongoQueryOperator.Not(this)))

  def unary_! : MongoOperatorsFilter[T] = negated

  def toOperatorsBson: BsonDocument = {
    val doc = new BsonDocument
    operators.foreach { op =>
      if (doc.containsKey(op.rawOperator)) {
        throw new IllegalArgumentException(s"duplicate query operator ${op.rawOperator}")
      }
      doc.put(op.rawOperator, op.toBson)
    }
    doc
  }
}

sealed trait MongoDocumentFilter[E] extends MongoFilter[E] {

  import MongoDocumentFilter._

  def and(other: MongoDocumentFilter[E]): MongoDocumentFilter[E] = (this, other) match {
    case (Empty(), other) => other
    case (thiz, Empty()) => thiz
    case (And(thisFilters), And(otherFilters)) => And(thisFilters ++ otherFilters)
    case (And(thisFilters), other) => And(thisFilters :+ other)
    case (thiz, And(otherFilters)) => And(thiz +: otherFilters)
    case _ => And(Vector(this, other))
  }

  def &&(other: MongoDocumentFilter[E]): MongoDocumentFilter[E] = and(other)

  def or(other: MongoDocumentFilter[E]): MongoDocumentFilter[E] = (this, other) match {
    case (Empty(), _) | (_, Empty()) => empty
    case (Or(thisFilters), Or(otherFilters)) => Or(thisFilters ++ otherFilters)
    case (Or(thisFilters), other) => Or(thisFilters :+ other)
    case (thiz, Or(otherFilters)) => Or(thiz +: otherFilters)
    case _ => Or(Vector(this, other))
  }

  def ||(other: MongoDocumentFilter[E]): MongoDocumentFilter[E] = or(other)

  final def toFilterBson(prefixPath: Opt[String]): BsonDocument = {
    val docs = new BsonArray
    addToFilters(prefixPath, docs)
    docs.size match {
      case 0 => Bson.document()
      case 1 => docs.get(0).asDocument
      case _ => Bson.document(Bson.And, docs)
    }
  }

  private def addToFilters(prefixPath: Opt[String], filterDocs: BsonArray): Unit = {
    def fullPath(suffix: String): String =
      prefixPath.fold(suffix)(_ + "." + suffix)

    def findFilterDoc(path: String): BsonDocument =
      filterDocs.iterator.asScala.map(_.asDocument)
        .findOpt(doc => !doc.containsKey(path) && doc.keySet.asScala.forall(k => !k.startsWith("$")))
        .getOrElse(Bson.document().setup(filterDocs.add))

    this match {
      case Empty() =>
      case And(filters) => filters.foreach(_.addToFilters(prefixPath, filterDocs))
      case Or(filters) => filterDocs.add(Bson.document(Bson.Or, Bson.array(filters.iterator.map(_.toFilterBson(prefixPath)))))
      case Nor(filters) => filterDocs.add(Bson.document(Bson.Nor, Bson.array(filters.iterator.map(_.toFilterBson(prefixPath)))))

      case PropertyValueFilter(prop, filter) => filter match {
        case docFilter: MongoDocumentFilter[_] =>
          docFilter.addToFilters(fullPath(prop.propertyPath).opt, filterDocs)

        case opFilter: MongoOperatorsFilter[_] =>
          val path = fullPath(prop.propertyPath)
          findFilterDoc(path).put(path, opFilter.toOperatorsBson)
      }
    }
  }
}
object MongoDocumentFilter {
  private[this] val reusableEmpty = Empty()

  def empty[E]: MongoDocumentFilter[E] = reusableEmpty.asInstanceOf[MongoDocumentFilter[E]]

  final case class Empty[E]() extends MongoDocumentFilter[E]
  final case class And[E](filters: Vector[MongoDocumentFilter[E]]) extends MongoDocumentFilter[E]
  final case class Or[E](filters: Vector[MongoDocumentFilter[E]]) extends MongoDocumentFilter[E]
  final case class Nor[E](filters: Vector[MongoDocumentFilter[E]]) extends MongoDocumentFilter[E]
  //NOTE: $not is NOT allowed as a toplevel operator

  final case class PropertyValueFilter[E, T](
    property: MongoPropertyRef[E, T],
    filter: MongoFilter[T]
  ) extends MongoDocumentFilter[E]

  def subtypeFilter[E, T](prefix: MongoRef[E, T], caseFieldName: String, caseNames: List[String]): MongoDocumentFilter[E] =
    MongoRef.caseNameRef(prefix, caseFieldName).in(caseNames)
}
