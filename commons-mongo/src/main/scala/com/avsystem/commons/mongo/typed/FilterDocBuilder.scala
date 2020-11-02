package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.mongo.typed.MongoFilter._
import org.bson.{BsonArray, BsonDocument}

import scala.annotation.tailrec

private final class FilterDocBuilder(prefixPath: Opt[String], filterDocs: BsonArray) {
  private def appendToPrefix(suffix: String): FilterDocBuilder = {
    val newPrefix = prefixPath.fold(suffix)(_ + "." + suffix)
    new FilterDocBuilder(newPrefix.opt, filterDocs)
  }

  @tailrec def addImpliedFilters(ref: MongoRef[_, _]): Unit = ref match {
    case MongoRef.SelfRef(_) =>

    case MongoRef.SelfAsSubtype(fullRef, caseFieldName, caseNames, _) =>
      addFilter(MongoFilter.subtypeFilter(fullRef, caseFieldName, caseNames))

    case MongoRef.PropertyAsSubtype(prefix, caseFieldName, caseNames, _) =>
      addFilter(MongoFilter.subtypeFilter(prefix, caseFieldName, caseNames))
      addImpliedFilters(prefix)

    case MongoRef.GetFromOptional(prefix, _, optionLike) =>
      appendToPrefix(prefix.filterPath).addOperator(MongoQueryOperator.Ne(optionLike.none, prefix.format))
      addImpliedFilters(prefix)

    case MongoRef.FieldRef(prefix, _, _, _) =>
      addImpliedFilters(prefix)

    case MongoRef.ArrayIndexRef(prefix, _, _) =>
      addImpliedFilters(prefix)
  }

  private def addOperator(op: MongoQueryOperator[_]): Unit = {
    val path = prefixPath.getOrElse(throw new IllegalArgumentException(
      "cannot add MongoOperatorsFilter to toplevel filter document without prefix path"))

    @tailrec def loop(idx: Int): Unit =
      if (idx < filterDocs.size) {
        val filterDoc = filterDocs.get(idx).asDocument
        val opsDoc = filterDoc.get(path) match {
          case null => new BsonDocument().setup(filterDoc.put(path, _))
          case doc => doc.asDocument
        }
        val opBson = op.toBson
        opsDoc.get(op.rawOperator) match {
          case null => opsDoc.put(op.rawOperator, opBson)
          case otherBson if otherBson != opBson => loop(idx + 1)
          case _ => // this exact operator is already in place, do nothing
        }
      } else {
        filterDocs.add(new BsonDocument(path, new BsonDocument(op.rawOperator, op.toBson)))
      }
    loop(0)
  }

  def addFilter(filter: MongoFilter[_]): Unit = filter match {
    case Empty() =>

    case And(filters) =>
      filters.foreach(addFilter)

    case Or(filters) =>
      filterDocs.add(Bson.document(Bson.Or, Bson.array(filters.iterator.map(_.toFilterBson(prefixPath, Set.empty)))))

    case Nor(filters) =>
      filterDocs.add(Bson.document(Bson.Nor, Bson.array(filters.iterator.map(_.toFilterBson(prefixPath, Set.empty)))))

    case PropertyValueFilter(ref, filter) =>
      appendToPrefix(ref.filterPath).addFilter(filter)
      addImpliedFilters(ref)

    case MongoOperatorsFilter(operators) =>
      operators.foreach(addOperator)
  }

  def finalDoc: BsonDocument = filterDocs.size match {
    case 0 => Bson.document()
    case 1 => filterDocs.get(0).asDocument
    case _ => Bson.document(Bson.And, filterDocs)
  }
}
