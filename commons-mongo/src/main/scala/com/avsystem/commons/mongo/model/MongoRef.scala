package com.avsystem.commons
package mongo.model

import com.avsystem.commons.annotation.macroPrivate
import com.avsystem.commons.macros.serialization.MongoMacros
import org.bson.BsonType

sealed trait MongoProjection[E, T]

sealed trait MongoRef[E, T] extends MongoProjection[E, T] { self =>
  def format: MongoFormat[T]

  // this macro effectively calls `caseRefFor` while doing some additional static checks
  def ref[T0](fun: T => T0): MongoRef[E, T0] = macro MongoMacros.refImpl

  def andThen[T0](other: MongoRef[T, T0]): MongoRef[E, T0] =
    MongoRef.AndThen(this, other)

  // this macro effectively calls `fieldRefFor` while doing some additional static checks
  def as[C <: T]: MongoRef[E, C] = macro MongoMacros.asSubtype[C]

  def satisfies(subQuery: MongoCondition[T]): MongoCondition[E] = subQuery.on(this)

  def exists: MongoCondition[E] =
    MongoCondition.ValueSatisfies(this, MongoRefCondition.Exists(true))

  def exists(exists: Boolean): MongoCondition[E] =
    MongoCondition.ValueSatisfies(this, MongoRefCondition.Exists(exists))

  def hasType(bsonType: BsonType): MongoCondition[E] =
    MongoCondition.ValueSatisfies(this, MongoRefCondition.Type(bsonType))

  def is(value: T): MongoCondition[E] =
    MongoCondition.ValueSatisfies(this, MongoRefCondition.Eq(value))

  def isNot(value: T): MongoCondition[E] =
    MongoCondition.ValueSatisfies(this, MongoRefCondition.Ne(value))

  // this macro effectively calls `caseConditionFor` while doing some additional static checks
  def is[C <: T]: MongoCondition[E] = macro MongoMacros.isSubtype[C]

  def in(values: Iterable[T]): MongoCondition[E] =
    MongoCondition.ValueSatisfies(this, MongoRefCondition.In(values))

  def in(values: T*): MongoCondition[E] = in(values)

  def notIn(values: Iterable[T]): MongoCondition[E] =
    MongoCondition.ValueSatisfies(this, MongoRefCondition.Nin(values))

  def notIn(values: T*): MongoCondition[E] = notIn(values)

  def >(value: T): MongoCondition[E] =
    MongoCondition.ValueSatisfies(this, MongoRefCondition.Gt(value))

  def >=(value: T): MongoCondition[E] =
    MongoCondition.ValueSatisfies(this, MongoRefCondition.Gte(value))

  def <(value: T): MongoCondition[E] =
    MongoCondition.ValueSatisfies(this, MongoRefCondition.Lt(value))

  def <=(value: T): MongoCondition[E] =
    MongoCondition.ValueSatisfies(this, MongoRefCondition.Lte(value))

  // The format passed as implicit is not used in runtime but serves as a static check
  // to detect situations where someone forgot about MongoDataCompanion on their data type
  // (fields that only have GenCodec are valid and their fallback format is MongoFormat.Leaf)
  @macroPrivate def fieldRefFor[T0](scalaFieldName: String)(implicit f: MongoAdtFormat[_ >: T]): MongoRef[E, T0] =
    format.assumeAdt.fieldRefFor(this, scalaFieldName)

  @macroPrivate def caseRefFor[C <: T : ClassTag]: MongoRef[E, C] =
    format.assumeUnion.caseRefFor(this, classTag[C].runtimeClass.asInstanceOf[Class[C]])

  @macroPrivate def caseConditionFor[C <: T: ClassTag]: MongoCondition[E] =
    format.assumeUnion.caseConditionFor(this, classTag[C].runtimeClass.asInstanceOf[Class[C]])
}
object MongoRef {
  implicit class IterableRefOps[C[X] <: Iterable[X], E, T](private val ref: MongoRef[E, C[T]]) extends AnyVal {
    def hasSize(size: Int): MongoCondition[E] =
      MongoCondition.ValueSatisfies(ref, MongoRefCondition.HasSize[C, T](size))

    def exists(condition: MongoCondition[T]): MongoCondition[E] =
      MongoCondition.ValueSatisfies(ref, MongoRefCondition.ElemMatch[C, T](condition))

    def forall(condition: MongoCondition[T]): MongoCondition[E] =
      MongoCondition.ValueSatisfies(ref, MongoRefCondition.All[C, T](condition))

    def contains(value: T): MongoCondition[E] =
      exists(SelfRef[T](ref.format.assumeCollection.elementFormat).is(value))

    def containsAny(values: Iterable[T]): MongoCondition[E] =
      exists(SelfRef[T](ref.format.assumeCollection.elementFormat).in(values))

    def containsAny(values: T*): MongoCondition[E] =
      containsAny(values)
  }

  // Deliberately not calling this IdentityRef so that it doesn't get confused with IdRef (for database ID field)
  final case class SelfRef[T](
    format: MongoFormat[T]
  ) extends MongoRef[T, T]

  final case class FieldRef[E, E0, T](
    prefix: MongoRef[E, E0],
    fieldName: Opt[String], // raw name, Opt.Empty in case of a transparent wrapper
    format: MongoFormat[T]
  ) extends MongoRef[E, T]

  final case class AndThen[E, E0, T](
    prefix: MongoRef[E, E0],
    suffix: MongoRef[E0, T]
  ) extends MongoRef[E, T] {
    def format: MongoFormat[T] = suffix.format
  }

  final case class AsSubtype[E, T0, T <: T0](
    ref: MongoRef[E, T0],
    caseFieldName: String,
    caseNames: Iterable[String],
    format: MongoFormat[T]
  ) extends MongoRef[E, T]
}

sealed trait MongoRefCondition[T]
object MongoRefCondition {
  final case class Exists[T](exists: Boolean) extends MongoRefCondition[T]
  final case class Type[T](bsonType: BsonType) extends MongoRefCondition[T]
  final case class Eq[T](value: T) extends MongoRefCondition[T]
  final case class Ne[T](value: T) extends MongoRefCondition[T]
  final case class In[T](values: Iterable[T]) extends MongoRefCondition[T]
  final case class Nin[T](values: Iterable[T]) extends MongoRefCondition[T]
  final case class Lt[T](value: T) extends MongoRefCondition[T]
  final case class Lte[T](value: T) extends MongoRefCondition[T]
  final case class Gt[T](value: T) extends MongoRefCondition[T]
  final case class Gte[T](value: T) extends MongoRefCondition[T]
  final case class HasSize[C[X] <: Iterable[X], T](size: Int) extends MongoRefCondition[C[T]]
  final case class ElemMatch[C[X] <: Iterable[X], T](condition: MongoCondition[T]) extends MongoRefCondition[C[T]]
  final case class All[C[X] <: Iterable[X], T](condition: MongoCondition[T]) extends MongoRefCondition[C[T]]
}

sealed trait MongoCondition[E] {
  def unary_! : MongoCondition[E] = MongoCondition.Not(this)
  def &&(other: MongoCondition[E]): MongoCondition[E] = MongoCondition.And(this, other)
  def ||(other: MongoCondition[E]): MongoCondition[E] = MongoCondition.Or(this, other)

  def on[E0](prefix: MongoRef[E0, E]): MongoCondition[E0] = MongoCondition.OnPrefix(prefix, this)
}
object MongoCondition {
  final case class Constant[E](value: Boolean) extends MongoCondition[E]
  final case class OnPrefix[E, E0](prefix: MongoRef[E, E0], condition: MongoCondition[E0]) extends MongoCondition[E]
  final case class And[E](left: MongoCondition[E], right: MongoCondition[E]) extends MongoCondition[E]
  final case class Or[E](left: MongoCondition[E], right: MongoCondition[E]) extends MongoCondition[E]
  final case class Not[E](condition: MongoCondition[E]) extends MongoCondition[E]

  final case class ValueSatisfies[E, T](
    ref: MongoRef[E, T],
    refCondition: MongoRefCondition[T]
  ) extends MongoCondition[E]

  final case class IsSubtype[E, T](
    ref: MongoRef[E, T],
    caseFieldName: String,
    caseNames: Iterable[String]
  ) extends MongoCondition[E]

  def True[E]: MongoCondition[E] = Constant(true)
  def False[E]: MongoCondition[E] = Constant(false)
}
