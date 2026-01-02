package com.avsystem.commons
package mongo.typed

import com.avsystem.commons.annotation.{explicitGenerics, macroPrivate}
import com.avsystem.commons.macros.serialization.MongoMacros

trait DataRefDsl[E, T] {
  // convenience type alias
  type Ref[T0] = MongoPropertyRef[E, T0]

  // ThisRef = MongoPropertyRef for MongoPropertyRef and MongoRef for all other types
  // This makes it possible to refine the result type of `as`, `compose`, `andThen` etc. in MongoPropertyRef
  // TODO: can we redesign this hierarchy to get rid of this abstraction and simplify things?
  type ThisRef[E0, T0] <: MongoRef[E0, T0]
  def SelfRef: ThisRef[E, T]

  // called by .ref macro to ensure that the source type is not opaque and inner references are possible
  @macroPrivate def asAdtRef(implicit ev: IsMongoAdtOrSubtype[T]): ThisRef[E, T] = SelfRef

  /** A macro that interprets an anonymous function as a [[MongoPropertyRef]].
    *
    * Let's define a MongoDB entity:
    * {{{
    *   case class Entity(
    *     id: String,
    *     number: Int,
    *     data: Data,
    *     dataOpt: Opt[Data],
    *     dataList: List[Data],
    *     dataMap: Map[String, Data]
    *   ) extends MongoEntity[String],
    *   object Entity extends MongoEntityCompanion[Entity]
    *
    *   case class Data(
    *     value: Int,
    *     complexData: Map[String, List[Opt[Int]]]
    *   )
    *   object Data extends MongoDataCompanion[Data]
    * }}}
    *
    * The `.ref` macro is available on its companion object.
    *
    * The function may be a reference to one of its fields:
    * {{{
    *   val intRef: MongoPropertyRef[Entity, Int] =
    *     Entity.ref(_.int)
    *   val dataRef: MongoPropertyRef[Entity, Data] =
    *     Entity.ref(_.data)
    * }}}
    *
    * Chaining is also possible:
    *
    * {{{
    *   val dataValueRef: MongoPropertyRef[Entity, Int] =
    *     Entity.ref(_.data.value)
    * }}}
    *
    * When `T` is an `Option`, `Opt`, or similar `Option`-like type, the function may refer its `.get` method to return
    * a reference to its inner value.
    *
    * {{{
    *   val dataRef: MongoPropertyRef[Entity, Data] =
    *     Entity.ref(_.dataOpt.get)
    * }}}
    *
    * When `T` is a collection, the function may call its `apply` method to refer to an element at specific index. Also,
    * `.head` may be used as an alias for `.apply(0)`.
    *
    * {{{
    *   val firstDataRef: MongoPropertyRef[Entity, Data] =
    *     Entity.ref(_.dataList.head)
    *   val secondDataRef: MongoPropertyRef[Entity, Data] =
    *     Entity.ref(_.dataList(1))
    * }}}
    *
    * When `T` is a map, the function may call its `apply` method to refer to a value at specific key.
    *
    * {{{
    *   val dataAtOneRef: MongoPropertyRef[Entity, Data] =
    *     Entity.ref(_.dataMap("one"))
    * }}}
    *
    * Now consider a MongoDB entity expressed as a sealed hierarchy with `@flatten` annotation:
    *
    * {{{
    *   @flatten sealed trait UnionEntity extends MongoEntity[UnionEntity] {
    *     def id: String
    *   }
    *   sealed trait HasNumber extends UnionEntity {
    *     def number: Int
    *   }
    *   case class FirstCase(id: String, flag: Boolean)
    *     extends UnionEntity
    *   case class SecondCase(id: String, number: Int, num: Double)
    *     extends HasNumber
    *   case class ThirdCase(id: String, number: Int, data: Data)
    *     extends HasNumber
    *   object UnionEntity extends MongoEntityCompanion[UnionEntity]
    * }}}
    *
    * The function passed to `.ref` macro may now refer to fields shared by all case classes (represented as abstract
    * `def`s on the sealed trait):
    *
    * {{{
    *   val idRef: MongoPropertyRef[UnionEntity, String] =
    *     UnionEntity.ref(_.id)
    * }}}
    *
    * You may also access fields of individual case classes by "narrowing" the reference explicitly to one particular
    * case class:
    *
    * {{{
    *   val flagRef: MongoPropertyRef[UnionEntity, Boolean] =
    *     UnionEntity.ref(_.as[CaseOne].flag)
    * }}}
    *
    * The same may be done for a subset of case classes sharing some common field. This subset must be expressed with an
    * intermediate sealed trait, like `HasNumber` in the above example:
    *
    * {{{
    *   val numberRef: MongoPropertyRef[UnionEntity, Int] =
    *     UnionEntity.ref(_.as[HasNumber].number)
    * }}}
    *
    * Finally, you can chain all of the above references into more complex paths:
    *
    * {{{
    *   val deeplyNestedRef: MongoPropertyRef[UnionEntity, Int] =
    *     UnionEntity.ref(_.as[ThirdCase].data.complexData("key").head.get)
    * }}}
    */
  def ref[T0](fun: T => T0): MongoPropertyRef[E, T0] = macro MongoMacros.refImpl

  /** Given a MongoDB union data type (defined with a sealed hierarchy with `@flatten` annotation), you can narrow it to
    * one of its case classes or intermediate sealed traits.
    *
    * {{{
    *   @flatten sealed trait UnionEntity extends MongoEntity[UnionEntity] {
    *     def id: String
    *   }
    *   sealed trait HasNumber extends UnionEntity {
    *     def number: Int
    *   }
    *   case class FirstCase(id: String, flag: Boolean)
    *     extends UnionEntity
    *   case class SecondCase(id: String, number: Int, num: Double)
    *     extends HasNumber
    *   case class ThirdCase(id: String, number: Int, data: Data)
    *     extends HasNumber
    *   object UnionEntity extends MongoEntityCompanion[UnionEntity]
    *
    *   val thirdCaseRef: MongoRef[UnionEntity, ThirdCase] =
    *     UnionEntity.as[ThirdCase]
    *   val hasNumberRef: MongoRef[UnionEntity, HasNumber] =
    *     UnionEntity.as[HasNumber]
    * }}}
    *
    * You can use such "narrowed" reference as a prefix for accessing [[MongoPropertyRef]]s using the [[ref]] macro,
    * e.g. `thirdCaseRef.ref(_.data)`. You can also use it as a [[MongoProjection]] passed to one of
    * [[TypedMongoCollection]] methods. Note that in such case the projection also serves as a filter, limiting the
    * results of the query only to selected cases.
    */
  @explicitGenerics
  def as[C <: T]: ThisRef[E, C] = macro MongoMacros.asSubtype[C]

  /** Macro for obtaining a [[MongoDocumentFilter]] (condition) which is satisfied only by some specific subtype of an
    * entity type. The entity must be a sealed trait/class and the subtype must be either one of its case classes or an
    * intermediate sealed trait extended by some subset of its case classes.
    *
    * {{{
    *   @flatten sealed trait UnionEntity extends MongoEntity[UnionEntity] {
    *     def id: String
    *   }
    *   sealed trait HasNumber extends UnionEntity {
    *     def number: Int
    *   }
    *   case class FirstCase(id: String, flag: Boolean)
    *     extends UnionEntity
    *   case class SecondCase(id: String, number: Int, num: Double)
    *     extends HasNumber
    *   case class ThirdCase(id: String, number: Int, data: Data)
    *     extends HasNumber
    *   object UnionEntity extends MongoEntityCompanion[UnionEntity]
    *
    *   val isThirdCase: MongoDocumentFilter[UnionEntity] =
    *     UnionEntity.is[ThirdCase]
    *   val hasNumber: MongoDocumentFilter[UnionEntity] =
    *     UnionEntity.is[HasNumber]
    * }}}
    */
  @explicitGenerics
  def is[C <: T]: MongoDocumentFilter[E] = macro MongoMacros.isSubtype[C]

  /** A negated version of [[is]].
    */
  @explicitGenerics
  def isNot[C <: T]: MongoDocumentFilter[E] = macro MongoMacros.isNotSubtype[C]
}

trait DataTypeDsl[T] extends DataRefDsl[T, T] {
  type ThisRef[E0, T0] = MongoRef[E0, T0]
}
