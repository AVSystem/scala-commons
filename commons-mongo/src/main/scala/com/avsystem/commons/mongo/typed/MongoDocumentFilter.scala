package com.avsystem.commons
package mongo.typed

import org.bson.{BsonArray, BsonDocument}

sealed trait MongoFilter[T] {
  def on[E](prefix: MongoPropertyRef[E, T]): MongoDocumentFilter[E] =
    prefix.impliedFilter && MongoDocumentFilter.PropertyValueFilter(prefix, this)

  final def toBson: BsonDocument = this match {
    case docFilter: MongoDocumentFilter[T] => docFilter.toFilterBson(Opt.Empty)
    case opsFilter: MongoOperatorsFilter[T] => opsFilter.toOperatorsBson
  }
}

object MongoFilter {
  def creator[T: MongoFormat]: Creator[T] = new Creator(MongoFormat[T])

  class Creator[T](val format: MongoFormat[T])
    extends QueryOperatorsDsl[T, MongoOperatorsFilter[T]] with DataTypeDsl[T] {

    type ThisRef[C <: T] = MongoDataRef[T, C]

    protected def thisRef: ThisRef[T] =
      MongoRef.SelfRef(format.assumeAdt)

    protected def wrapQueryOperator(op: MongoQueryOperator[T]): MongoOperatorsFilter[T] =
      MongoOperatorsFilter(Seq(op))

    def satisfiesOperators(operators: MongoQueryOperator.Creator[T] => Seq[MongoQueryOperator[T]]): MongoOperatorsFilter[T] =
      MongoOperatorsFilter(operators(new MongoQueryOperator.Creator(format)))
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

  def subtypeFilter[E, T](prefix: MongoRef[E, T], caseFieldName: String, caseNames: List[String]): MongoDocumentFilter[E] = {
    val operator = caseNames match {
      case List(single) => MongoQueryOperator.Eq(single, MongoFormat[String])
      case multiple => MongoQueryOperator.In(multiple, MongoFormat[String])
    }
    PropertyValueFilter(MongoRef.caseNameRef(prefix, caseFieldName), MongoOperatorsFilter(Seq(operator)))
  }
}
