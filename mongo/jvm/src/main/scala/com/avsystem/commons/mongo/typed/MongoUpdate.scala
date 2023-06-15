package com.avsystem.commons
package mongo.typed

import org.bson.BsonDocument

/**
  * Represents an update of a MongoDB value. Usually, this is a [[MongoDocumentUpdate]].
  * The base `MongoUpdate` type is used in rare situations where the value being updated may not be a document.
  *
  * @tparam T type of the value being updated
  */
sealed trait MongoUpdate[T] {

  import MongoUpdate._

  def on[E](property: MongoPropertyRef[E, T]): MongoDocumentUpdate[E] =
    PropertyUpdate(property, this)

  protected def fillUpdateDoc(pathOpt: Opt[String], doc: BsonDocument, arrayFilters: JList[BsonDocument]): Unit = this match {
    case MultiUpdate(updates) =>
      updates.foreach(_.fillUpdateDoc(pathOpt, doc, arrayFilters))

    case PropertyUpdate(property, update) =>
      val propPath = property.rawPath
      val newPath = pathOpt.fold(propPath)(_ + MongoPropertyRef.Separator + propPath)
      update.fillUpdateDoc(newPath.opt, doc, arrayFilters)

    case OperatorUpdate(op) =>
      // the way MongoDocumentUpdate uses fillUpdateDoc makes this safe
      val path = pathOpt.getOrElse(throw new IllegalArgumentException("update document without prefix path"))
      if (!doc.containsKey(op.rawOperator)) {
        doc.put(op.rawOperator, new BsonDocument)
      }
      val opDoc = doc.get(op.rawOperator).asDocument
      if (!opDoc.containsKey(path)) {
        opDoc.put(path, op.toBson)
      } else {
        throw new IllegalArgumentException(s"duplicate update operator ${op.rawOperator} on field $path")
      }

    case uae: UpdateArrayElements[_, _] =>
      val path = pathOpt.getOrElse(throw new IllegalArgumentException("update document without prefix path"))

      val rawQualifier = uae.qualifier match {
        case ArrayElementsQualifier.FirstMatching() => "$"
        case ArrayElementsQualifier.Each() => "$[]"
        case ArrayElementsQualifier.Filtered(filter) =>
          val identifier = s"filter${arrayFilters.size}"
          val (name, query) = filter match {
            case MongoFilter.PropertyValueFilter(prop, propertyFilter) =>
              // https://www.mongodb.com/docs/manual/reference/operator/update/positional-filtered/#update-all-documents-that-match-arrayfilters-in-an-array
              identifier + MongoPropertyRef.Separator + prop.rawPath -> propertyFilter
            case filter =>
              identifier -> filter
          }
          arrayFilters.add(Bson.document(name, query.toBson))
          s"$$[$identifier]"
      }

      val newPath = path + MongoPropertyRef.Separator + rawQualifier
      uae.update.fillUpdateDoc(newPath.opt, doc, arrayFilters)
  }
}

object MongoUpdate {
  def creator[T: MongoFormat]: Creator[T] = new Creator(MongoFormat[T])

  class Creator[T](val format: MongoFormat[T])
    extends UpdateOperatorsDsl[T, MongoUpdate[T]] with DataTypeDsl[T] {

    def SelfRef: MongoRef[T, T] =
      MongoRef.RootRef(format.assumeAdt)

    protected def wrapUpdate(update: MongoUpdate[T]): MongoUpdate[T] = update
  }

  def empty[T]: MongoDocumentUpdate[T] = MongoDocumentUpdate.empty

  final case class OperatorUpdate[T](
    operator: MongoUpdateOperator[T]
  ) extends MongoUpdate[T]

  final case class MultiUpdate[E](
    propUpdates: Vector[PropertyUpdate[E, _]]
  ) extends MongoDocumentUpdate[E]

  final case class PropertyUpdate[E, T](
    property: MongoPropertyRef[E, T],
    update: MongoUpdate[T]
  ) extends MongoDocumentUpdate[E]

  final case class UpdateArrayElements[C[X] <: Iterable[X], T](
    update: MongoUpdate[T],
    qualifier: ArrayElementsQualifier[T]
  ) extends MongoUpdate[C[T]]

  sealed abstract class ArrayElementsQualifier[T]
  object ArrayElementsQualifier {
    case class FirstMatching[T]() extends ArrayElementsQualifier[T]
    case class Each[T]() extends ArrayElementsQualifier[T]
    case class Filtered[T](filter: MongoFilter[T]) extends ArrayElementsQualifier[T]
  }
}

/**
  * Represents a [[https://docs.mongodb.com/manual/tutorial/update-documents/ MongoDB update document]].
  *
  * Examples:
  * {{{
  *   case class MyEntity(
  *     id: String,
  *     int: Int,
  *     str: String,
  *     intList: List[Int]
  *   ) extends MongoEntity[String]
  *   object MyEntity extends MongoEntityCompanion[MyEntity]
  *
  *   // {"$$set": {"int": 0}}
  *   MyEntity.ref(_.int).set(0)
  *
  *   // {"$$inc": {"int": 1}}
  *   MyEntity.ref(_.int).inc(1)
  *
  *   // {"$$set": {"int": 0, "str": "foo"}}
  *   MyEntity.ref(_.int).set(0) and MyEntity.ref(_.str).set("foo")
  *
  *   // {"$$inc": {"int": 1}, {"$$set": {"str": "foo"}}
  *   MyEntity.ref(_.int).inc(1) and MyEntity.ref(_.str).set("foo")
  *
  * }}}
  *
  * @tparam E type of the document being updated
  */
sealed trait MongoDocumentUpdate[E] extends MongoUpdate[E] {

  import MongoUpdate._

  // distinction between MultiUpdate and PropertyUpdate is mostly to avoid creating too many Vectors
  def and(other: MongoDocumentUpdate[E]): MongoDocumentUpdate[E] = (this, other) match {
    case (MultiUpdate(propUpdates), propUpdate: PropertyUpdate[E, _]) =>
      MultiUpdate(propUpdates :+ propUpdate)
    case (propUpdate: PropertyUpdate[E, _], MultiUpdate(propUpdates)) =>
      MultiUpdate(propUpdate +: propUpdates)
    case (propUpdate1: PropertyUpdate[E, _], propUpdate2: PropertyUpdate[E, _]) =>
      MultiUpdate(Vector(propUpdate1, propUpdate2))
    case (MultiUpdate(propUpdates1), MultiUpdate(propUpdates2)) =>
      MultiUpdate(propUpdates1 ++ propUpdates2)
  }

  def &&(other: MongoDocumentUpdate[E]): MongoDocumentUpdate[E] = and(other)

  def toBsonAndArrayFilters: (BsonDocument, JList[BsonDocument]) = {
    val updateDoc = new BsonDocument
    val arrayFilters = new JLinkedList[BsonDocument]
    fillUpdateDoc(Opt.Empty, updateDoc, arrayFilters)
    (updateDoc, arrayFilters)
  }
}

object MongoDocumentUpdate {
  def empty[T]: MongoDocumentUpdate[T] = MongoUpdate.MultiUpdate(Vector.empty)
}
