package com.avsystem.commons
package mongo.typed

import org.bson.{BsonArray, BsonDocument}

/**
  * Represents a MongoDB filter. In particular, this may be a [[MongoDocumentFilter]] which can be used
  * directly in database queries.
  *
  * [[MongoFilter]] may also represent a filter for a non-document value. Such filters may be used e.g. in
  * queries that apply filters on values of an array.
  *
  * @tparam T type of the filtered value
  */
sealed trait MongoFilter[T] {

  import MongoFilter._

  def on[E](prefix: MongoPropertyRef[E, T]): MongoDocumentFilter[E] =
    PropertyValueFilter(prefix, this)

  final def toBson: BsonDocument = this match {
    case docFilter: MongoDocumentFilter[T] => docFilter.toFilterBson(Opt.Empty, Set.empty)
    case opsFilter: MongoOperatorsFilter[T] => opsFilter.toOperatorsBson
  }
}

object MongoFilter {
  def creator[T: MongoFormat]: Creator[T] = new Creator(MongoFormat[T])

  class Creator[T](val format: MongoFormat[T])
    extends QueryOperatorsDsl[T, MongoOperatorsFilter[T]] with DataTypeDsl[T] {

    def SelfRef: MongoRef[T, T] =
      MongoRef.RootRef(format.assumeAdt)

    protected def wrapQueryOperator(op: MongoQueryOperator[T]): MongoOperatorsFilter[T] =
      MongoOperatorsFilter(Seq(op))

    /** See [[MongoPropertyRef.satisfiesOperators]] */
    def satisfiesOperators(operators: MongoQueryOperator.Creator[T] => Seq[MongoQueryOperator[T]]): MongoOperatorsFilter[T] =
      MongoOperatorsFilter(operators(new MongoQueryOperator.Creator(format)))
  }

  private[this] val reusableEmpty = Empty()

  def empty[E]: MongoDocumentFilter[E] = reusableEmpty.asInstanceOf[MongoDocumentFilter[E]]
  def and[E](filters: MongoDocumentFilter[E]*): MongoDocumentFilter[E] = And(filters.toVector)
  def or[E](filters: MongoDocumentFilter[E]*): MongoDocumentFilter[E] = Or(filters.toVector)
  def nor[E](filters: MongoDocumentFilter[E]*): MongoDocumentFilter[E] = Nor(filters.toVector)

  final case class Empty[E]() extends MongoDocumentFilter[E]
  final case class And[E](filters: Vector[MongoDocumentFilter[E]]) extends MongoDocumentFilter[E]
  final case class Or[E](filters: Vector[MongoDocumentFilter[E]]) extends MongoDocumentFilter[E]
  final case class Nor[E](filters: Vector[MongoDocumentFilter[E]]) extends MongoDocumentFilter[E]
  //NOTE: $not is NOT allowed as a toplevel operator

  final case class PropertyValueFilter[E, T](
    property: MongoPropertyRef[E, T],
    filter: MongoFilter[T]
  ) extends MongoDocumentFilter[E]

  def subtypeFilter[E, T](prefix: MongoRef[E, T], caseFieldName: String, caseNames: List[String], negated: Boolean): MongoDocumentFilter[E] = {
    val operator = caseNames match {
      case List(single) if negated => MongoQueryOperator.Ne(single, MongoFormat[String])
      case List(single) => MongoQueryOperator.Eq(single, MongoFormat[String])
      case multiple if negated => MongoQueryOperator.Nin(multiple, MongoFormat[String])
      case multiple => MongoQueryOperator.In(multiple, MongoFormat[String])
    }
    PropertyValueFilter(MongoRef.caseNameRef(prefix, caseFieldName), MongoOperatorsFilter(Seq(operator)))
  }
}

/**
  * Represents a filter that applies a set of query operators on a value (e.g. `$$eq`, `$$lte`, etc.)
  * Every operator may be used at most once. Such filter usually translates into a BSON document that looks like this:
  *
  * {{{
  *   {"$$ne": 5, "$$gte": 0, ...more operators}
  * }}}
  *
  * @tparam T type of the filtered value
  */
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

/**
  * Represents a [[https://docs.mongodb.com/manual/tutorial/query-documents/ MongoDB query document]].
  *
  * A [[MongoDocumentFilter]] is usually built by using [[MongoPropertyRef]] and its API.
  *
  * {{{
  *   case class MyEntity(id: String, number: Int) extends MongoEntity[MyEntity]
  *   object MyEntity extends MongoEntityCompanion[MyEntity]
  *
  *   val filter: MongoDocumentFilter[MyEntity] =
  *     MyEntity.ref(_.id).is("ID") && MyEntity.ref(_.number) > 8
  * }}}
  *
  * NOTE: even though you can combine filters using logical `$$and` and `$$or` operators,
  * MongoDB does not allow using the `$$not` operator on this level.
  * It may only be specified for field-level filters.
  *
  * @tparam E type of the filtered value (that must translate into a document)
  */
sealed trait MongoDocumentFilter[E] extends MongoFilter[E] {

  import MongoFilter._

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

  final def toFilterBson(prefixPath: Opt[String], projectionRefs: Set[MongoRef[E, _]]): BsonDocument = {
    val builder = new FilterDocBuilder(prefixPath, new BsonArray)
    builder.addFilter(this)
    projectionRefs.foreach(builder.addImpliedFilters)
    builder.finalDoc
  }
}
