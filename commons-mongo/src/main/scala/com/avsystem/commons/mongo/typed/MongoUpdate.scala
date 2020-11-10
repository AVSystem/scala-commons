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

  protected def fillUpdateDoc(pathOpt: Opt[String], doc: BsonDocument): Unit = this match {
    case MultiUpdate(updates) =>
      updates.foreach(_.fillUpdateDoc(pathOpt, doc))

    case PropertyUpdate(property, update) =>
      val propPath = property.updatePath
      val newPath = pathOpt.fold(propPath)(_ + MongoPropertyRef.Separator + propPath)
      update.fillUpdateDoc(newPath.opt, doc)

    case OperatorsUpdate(operators) =>
      // the way MongoDocumentUpdate uses fillUpdateDoc makes this safe
      val path = pathOpt.getOrElse(throw new IllegalArgumentException("update document without prefix path"))
      operators.foreach { op =>
        if (!doc.containsKey(op.rawOperator)) {
          doc.put(op.rawOperator, new BsonDocument)
        }
        val opDoc = doc.get(op.rawOperator).asDocument
        if (!opDoc.containsKey(path)) {
          opDoc.put(path, op.toBson)
        } else {
          throw new IllegalArgumentException(s"duplicate update operator ${op.rawOperator} on field $path")
        }
      }
  }
}

object MongoUpdate {
  def empty[T]: MongoDocumentUpdate[T] = MongoDocumentUpdate.empty

  final case class OperatorsUpdate[T](
    operators: Vector[MongoUpdateOperator[T]]
  ) extends MongoUpdate[T]

  final case class MultiUpdate[E](
    propUpdates: Vector[PropertyUpdate[E, _]]
  ) extends MongoDocumentUpdate[E]

  final case class PropertyUpdate[E, T](
    property: MongoPropertyRef[E, T],
    update: MongoUpdate[T]
  ) extends MongoDocumentUpdate[E]
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

  def toBson: BsonDocument = {
    val updateDoc = new BsonDocument
    fillUpdateDoc(Opt.Empty, updateDoc)
    updateDoc
  }
}

object MongoDocumentUpdate {
  def empty[T]: MongoDocumentUpdate[T] = MongoUpdate.MultiUpdate(Vector.empty)
}
