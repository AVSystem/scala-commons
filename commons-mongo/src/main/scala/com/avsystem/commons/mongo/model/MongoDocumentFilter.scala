package com.avsystem.commons
package mongo.model

import com.avsystem.commons.annotation.macroPrivate
import com.avsystem.commons.mongo.BsonValueOutput
import org.bson.{BsonArray, BsonDocument, BsonType, BsonValue}

sealed trait MongoFilter[T] {
  def on[E](prefix: MongoPropertyRef[E, T]): MongoDocumentFilter[E] =
    prefix.satisfies(this)

  final def toBson: BsonDocument = this match {
    case docFilter: MongoDocumentFilter[T] => docFilter.toFilterDocument(Opt.Empty)
    case opsFilter: MongoOperatorsFilter[T] => opsFilter.toOperatorsBson
  }
}

object MongoFilter {
  def apply[T: MongoFormat]: Creator[T] = new Creator(MongoFormat[T])

  implicit def creatorForCollection[C[X] <: Iterable[X], T](creator: Creator[C[T]]): CreatorForCollection[C, T] =
    new CreatorForCollection(creator.format)

  class Creator[T](val format: MongoFormat[T]) extends AnyVal with HasRefMacros[T, T] {
    type ThisDataRef[C <: T] = MongoDataRef[T, C]

    @macroPrivate def thisDataRef: ThisDataRef[T] = MongoRef.SelfRef(format.assumeAdt)

    def exists: MongoOperatorsFilter[T] = exists(true)
    def exists(exists: Boolean): MongoOperatorsFilter[T] = satisfies(exists = exists)
    def hasType(bsonType: BsonType): MongoOperatorsFilter[T] = satisfies(bsonType = bsonType)
    def is(value: T): MongoOperatorsFilter[T] = satisfies(eq = value)
    def isNot(value: T): MongoOperatorsFilter[T] = satisfies(ne = value)
    def in(values: T*): MongoOperatorsFilter[T] = in(values)
    def in(values: Iterable[T]): MongoOperatorsFilter[T] = satisfies(in = values)
    def nin(values: T*): MongoOperatorsFilter[T] = nin(values)
    def nin(values: Iterable[T]): MongoOperatorsFilter[T] = satisfies(nin = values)
    def gt(value: T): MongoOperatorsFilter[T] = satisfies(gt = value)
    def >(value: T): MongoOperatorsFilter[T] = gt(value)
    def gte(value: T): MongoOperatorsFilter[T] = satisfies(gte = value)
    def >=(value: T): MongoOperatorsFilter[T] = gte(value)
    def lt(value: T): MongoOperatorsFilter[T] = satisfies(lt = value)
    def <(value: T): MongoOperatorsFilter[T] = lt(value)
    def lte(value: T): MongoOperatorsFilter[T] = satisfies(lte = value)
    def <=(value: T): MongoOperatorsFilter[T] = lte(value)

    def not(filter: Creator[T] => MongoOperatorsFilter[T]): MongoOperatorsFilter[T] =
      not(filter(this))

    def not(filter: MongoOperatorsFilter[T]): MongoOperatorsFilter[T] =
      filter.negated

    def rawFilter(bson: BsonDocument): MongoOperatorsFilter[T] =
      MongoOperatorsFilter.RawOperators(format, bson)

    def satisfies(
      exists: OptArg[Boolean] = OptArg.Empty,
      bsonType: OptArg[BsonType] = OptArg.Empty,
      eq: OptArg[T] = OptArg.Empty,
      ne: OptArg[T] = OptArg.Empty,
      gt: OptArg[T] = OptArg.Empty,
      gte: OptArg[T] = OptArg.Empty,
      lt: OptArg[T] = OptArg.Empty,
      lte: OptArg[T] = OptArg.Empty,
      in: OptArg[Iterable[T]] = OptArg.Empty,
      nin: OptArg[Iterable[T]] = OptArg.Empty,
      not: OptArg[MongoOperatorsFilter[T]] = OptArg.Empty
    ): MongoOperatorsFilter[T] =
      MongoOperatorsFilter.SimpleOperators(
        format,
        exists.toOpt,
        bsonType.toOpt,
        eq.toOpt,
        ne.toOpt,
        gt.toOpt,
        gte.toOpt,
        lt.toOpt,
        lte.toOpt,
        in.toOpt,
        nin.toOpt,
        not.toOpt
      )
  }

  class CreatorForCollection[C[X] <: Iterable[X], T](private val format: MongoFormat[C[T]]) extends AnyVal {
    def elementFormat: MongoFormat[T] = format.assumeCollection.elementFormat

    def size(size: Int): MongoOperatorsFilter[C[T]] =
      collectionSatisfies(size = size)

    def elemMatch(filter: Creator[T] => MongoFilter[T]): MongoOperatorsFilter[C[T]] =
      elemMatch(filter(new Creator(elementFormat)))

    def elemMatch(filter: MongoFilter[T]): MongoOperatorsFilter[C[T]] =
      collectionSatisfies(elemMatch = filter)

    def all(values: T*): MongoOperatorsFilter[C[T]] = all(values)

    def all(values: Iterable[T]): MongoOperatorsFilter[C[T]] =
      collectionSatisfies(all = values)

    def collectionSatisfies(
      exists: OptArg[Boolean] = OptArg.Empty,
      bsonType: OptArg[BsonType] = OptArg.Empty,
      eq: OptArg[C[T]] = OptArg.Empty,
      ne: OptArg[C[T]] = OptArg.Empty,
      in: OptArg[Iterable[C[T]]] = OptArg.Empty,
      nin: OptArg[Iterable[C[T]]] = OptArg.Empty,
      not: OptArg[MongoOperatorsFilter[C[T]]] = OptArg.Empty,
      size: OptArg[Int] = OptArg.Empty,
      elemMatch: OptArg[MongoFilter[T]] = OptArg.Empty,
      all: OptArg[Iterable[T]] = OptArg.Empty,
    ): MongoOperatorsFilter[C[T]] =
      MongoOperatorsFilter.ArrayOperators(
        format,
        elementFormat,
        MongoOperatorsFilter.SimpleOperators(
          format,
          exists.toOpt,
          bsonType.toOpt,
          eq.toOpt,
          ne.toOpt,
          Opt.Empty,
          Opt.Empty,
          Opt.Empty,
          Opt.Empty,
          in.toOpt,
          nin.toOpt,
          not.toOpt
        ),
        size.toOpt,
        elemMatch.toOpt,
        all.toOpt
      )
  }
}

sealed trait MongoOperatorsFilter[T] extends MongoFilter[T] {

  import MongoOperatorsFilter._

  def format: MongoFormat[T]

  def negated: MongoOperatorsFilter[T] =
    SimpleOperators(format, not = Opt(this))

  def unary_! : MongoOperatorsFilter[T] = negated

  final def toOperatorsBson: BsonDocument = {
    def write[X](value: X, format: MongoFormat[X]): BsonValue =
      BsonValueOutput.write(value)(format.codec)

    def writeSimpleOperators[X](doc: BsonDocument, operators: SimpleOperators[X]): Unit = {
      operators.exists.foreach(v => doc.put(Bson.Exists, Bson.boolean(v)))
      operators.bsonType.foreach(v => doc.put(Bson.Type, Bson.int(v.getValue)))
      operators.eq.foreach(v => doc.put(Bson.Eq, write(v, operators.format)))
      operators.ne.foreach(v => doc.put(Bson.Ne, write(v, operators.format)))
      operators.gt.foreach(v => doc.put(Bson.Gt, write(v, operators.format)))
      operators.gte.foreach(v => doc.put(Bson.Gte, write(v, operators.format)))
      operators.lt.foreach(v => doc.put(Bson.Lt, write(v, operators.format)))
      operators.lte.foreach(v => doc.put(Bson.Lte, write(v, operators.format)))
      operators.in.foreach(v => doc.put(Bson.In, Bson.array(v.iterator.map(write(_, operators.format)))))
      operators.nin.foreach(v => doc.put(Bson.Nin, Bson.array(v.iterator.map(write(_, operators.format)))))
      operators.not.foreach(v => doc.put(Bson.Not, v.toOperatorsBson))
    }

    this match {
      case simple: SimpleOperators[T] =>
        val doc = new BsonDocument
        writeSimpleOperators(doc, simple)
        doc

      case arrayOps: ArrayOperators[_, ct] =>
        val doc = new BsonDocument
        writeSimpleOperators(doc, arrayOps.simple)
        arrayOps.size.foreach(v => doc.put(Bson.Size, Bson.int(v)))
        arrayOps.elemMatch.foreach(v => doc.put(Bson.ElemMatch, v.toBson))
        arrayOps.all.foreach(v => doc.put(Bson.All, Bson.array(v.iterator.map(write(_, arrayOps.elementFormat)))))
        doc

      case raw: RawOperators[T] =>
        raw.bson
    }
  }
}

object MongoOperatorsFilter {
  final case class SimpleOperators[T](
    format: MongoFormat[T],
    exists: Opt[Boolean] = Opt.Empty,
    bsonType: Opt[BsonType] = Opt.Empty,
    eq: Opt[T] = Opt.Empty,
    ne: Opt[T] = Opt.Empty,
    gt: Opt[T] = Opt.Empty,
    gte: Opt[T] = Opt.Empty,
    lt: Opt[T] = Opt.Empty,
    lte: Opt[T] = Opt.Empty,
    in: Opt[Iterable[T]] = Opt.Empty,
    nin: Opt[Iterable[T]] = Opt.Empty,
    not: Opt[MongoOperatorsFilter[T]] = Opt.Empty
  ) extends MongoOperatorsFilter[T]

  final case class ArrayOperators[C[X] <: Iterable[X], T](
    format: MongoFormat[C[T]],
    elementFormat: MongoFormat[T],
    simple: SimpleOperators[C[T]],
    size: Opt[Int] = Opt.Empty,
    elemMatch: Opt[MongoFilter[T]] = Opt.Empty,
    all: Opt[Iterable[T]] = Opt.Empty,
  ) extends MongoOperatorsFilter[C[T]]

  final case class RawOperators[T](
    format: MongoFormat[T],
    bson: BsonDocument
  ) extends MongoOperatorsFilter[T]
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
    case (Empty(), _) | (_, Empty()) => Empty()
    case (Or(thisFilters), Or(otherFilters)) => Or(thisFilters ++ otherFilters)
    case (Or(thisFilters), other) => Or(thisFilters :+ other)
    case (thiz, Or(otherFilters)) => Or(thiz +: otherFilters)
    case _ => Or(Vector(this, other))
  }

  def ||(other: MongoDocumentFilter[E]): MongoDocumentFilter[E] = or(other)

  final def toFilterDocument(prefixPath: Opt[String]): BsonDocument = {
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
      case Or(filters) => filterDocs.add(Bson.document(Bson.Or, Bson.array(filters.iterator.map(_.toFilterDocument(prefixPath)))))
      case Nor(filters) => filterDocs.add(Bson.document(Bson.Nor, Bson.array(filters.iterator.map(_.toFilterDocument(prefixPath)))))

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
