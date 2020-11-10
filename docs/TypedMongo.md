# MongoDB API

The `commons-mongo` module contains various utilities that enhance standard Java & Scala drivers for MongoDB.
This documentation focuses on the `com.avsystem.commons.mongo.typed` package that contains a Scala-idiomatic and
typesafe layer over the Reactive Streams driver for MongoDB.

## Quickstart

### Defining an entity

```scala
import org.bson.types.ObjectId

case class MyEntity(
  id: ObjectId,
  int: Int,
  str: String
) extends MongoEntity[ObjectId]
object MyEntity extends MongoEntityCompanion[MyEntity]
```

### Setting up the client

```scala
import com.mongodb.reactivestreams.client.MongoClients

val client = MongoClients.create() // connects to localhost by default
val rawCollection = client.getDatabase("test").getCollection("myEntity")

val collection: TypedMongoCollection[MyEntity] = new TypedMongoCollection(rawCollection)
```

### Inserting documents

```scala
val entities = Seq(
  MyEntity(ObjectId.get(), 1, "first"),
  MyEntity(ObjectId.get(), 2, "second"),
  MyEntity(ObjectId.get(), 3, "third")
)

import monix.eval.Task

// every DB operation returns a Monix Task or Observable
val task: Task[Unit] =
  collection.insertMany(entities)

// use whatever Scheduler is appropriate in your context (like ExecutionContext for Futures)
import monix.execution.Scheduler.Implicits.global
// run the Task
task.foreach(_ => println("insert successful"))
```

### Finding documents by query

```scala
import monix.eval.Task
import monix.reactive.Observable

val observable: Observable[MyEntity] =
  collection.find(MyEntity.ref(_.int) > 1)

// use whatever Scheduler is appropriate in your context (like ExecutionContext for Futures)
import monix.execution.Scheduler.Implicits.global
// run the Observable
observable.foreach(entity => println(s"Found entity: $entity"))

// alternatively, collect all found entities into a List
val listTask: Task[List[MyEntity]] = observable.toListL
listTask.foreach(foundEntities => println(s"Found entities: $foundEntities"))
```

## Modeling entities

A MongoDB entity may be:
* a case class
* a sealed trait/class with `@flatten` annotation (see `GenCodec`'s [flat format](GenCodec.md#flat-format) for more details)

Also, every MongoDB entity must extend `MongoEntity` in order to specify the ID field and type. 
Its companion must also extend `MongoEntityCompanion`. This causes two typeclass instances (implicits) 
to be materialized for the entity type:

* `GenCodec` - used for serialization of the entity into BSON. See [`GenCodec`](GenCodec.md) for more details.
* `MongoAdtFormat` - captures the internal structure of the entity, making it possible to refer to properties of this
  entity in order to build queries, updates, sort orders, etc.

### Field types

Any type that has a `GenCodec` instance will be accepted as a field type in MongoDB entity.
However, the MongoDB API is aware of internal structure of some types, including:

* [embedded documents](#embedded-document-types) - serialized into BSON documents
* collections, i.e. any subtype of `scala.collection.Seq` or `scala.collection.Set` - serialized into BSON arrays
* maps, i.e. any subtype of `scala.collection.Map` - serialized into BSON documents
* option-like types, i.e. `Option`, `Opt`, `OptArg`, etc. - serialized into nullable values

Being aware of internal structure makes it possible to build queries, updates, projections, etc. that reach inside
these data types. For example, you can refer to a specific map entry in a query.

### Embedded document types

A type representing embedded document is very similar to an entity type, i.e. it must be:

* a case class
* a sealed trait/class with `@flatten` annotation

However, an embedded document is not a toplevel entity and does not need an ID. Therefore, it doesn't need to
extend `MongoEntity` and its companion must extend `MongoDataCompanion` rather than `MongoEntityCompanion.

```scala
case class MyEmbeddedDocument(
  int: Int,
  string: String
)
object MyEmbeddedDocument extends MongoDataCompanion[MyEmbeddedDocument]
```

**NOTE**: you could simply use `HasGenCodec` as the base companion class and `MyEmbeddedDocument` would be accepted
as entity field type. However, it would be impossible to build queries, updates, etc. referring to embedded document's
fields. In other words, the type would be valid but _opaque_.

### Optional fields

A field typed as an `Option`, `Opt`, `OptArg` or similar will serialize just like its wrapped value except that `null`
will be used to represent the absence of value. If you want to omit that `null`, effectively making the field optional
on BSON level, use one of the following:

* `@optionalField intOpt: Option[Int]`
* `@transientDefault intOpt: Option[Int] = None`
* `@transientDefault @whenAbsent(None) intOpt: Option[Int]`

**NOTE**: even when a field is absent in a MongoDB document, the database will pretend that it contains `null` 
while performing a query. Therefore, you can use `{"$eq": null}` queries to match _both_ absent and `null` values.

This is especially useful when introducing new fields into already existing entities. Using optional fields is an easy
way to keep serialization compatibility between subsequent versions.

## Core types

One of the improvements introduced by `commons-mongo` over the raw Reactive Streams driver are well typed
representations for field references, query documents, projections, update documents, sort orders, indices etc.
The raw API uses untyped strings or BSON objects in these situations.

Let's assume some example entities to be used throughout this section:

```scala
case class MyEntity(
  id: String,
  int: Int,
  str: String,
  strOpt: Option[String],
  intList: List[String],
  strMap: Map[String, String],
  data: MyData
) extends MongoEntity[String]
object MyEntity extends MongoEntityCompanion[MyEntity]

case class MyData(
  number: Double,
  flag: Boolean
)
object MyData extends MongoDataCompanion[MyData]

@flatten sealed trait MyUnionEntity extends MongoEntity[String]
sealed trait HasInt extends MyUnionEntity {
  def value: Int
}
case class FirstCase(id: String) extends MyUnionEntity
case class SecondCase(id: String, value: Int) extends HasInt
case class ThirdCase(id: String, value: Int, data: MyData) extends HasInt
object MyUnionEntity extends MongoEntityCompanion[MyUnionEntity]
```

### `MongoPropertyRef`

A `MongoPropertyRef[E, T]` is a reference to a property of type `T` inside a document of type `E`.
It may be a direct reference to a field or a more complex path referring to deeply nested parts of the document.
A `MongoPropertyRef` is usually obtained with the `.ref` macro which interprets a Scala-level lambda expression into
a property reference:

```scala
val IntRef: MongoPropertyRef[MyEntity, Int] = MyEntity.ref(_.int)
```

Using the `.ref` macro, you can:

* refer to case class fields, e.g. `MyEntity.ref(_.int)`
* refer to values inside option-like fields, e.g. `MyEntity.ref(_.strOpt.get)`
* refer to array elements, e.g. `MyEntity.ref(_.intList(2))`
* refer to map entries, e.g. `MyEntity.ref(_.strMap("key"))`
* refer to common fields in a sealed hierarchy, e.g. `MyUnionEntity.ref(_.id)`
* refer to fields of specific case classes in a sealed hierarchy, e.g. `MyUnionEntity.ref(_.as[ThirdCase].data)`
* refer to common fields of intermediate subtraits in a sealed hierarchy, e.g. `MyUnionEntity.ref(_.as[HasInt].value)`

You can freely combine all the above constructs into more deeply nested references.
For more examples, see the Scaladoc of the `.ref` macro or 
[tests](https://github.com/AVSystem/scala-commons/blob/mongo-api/commons-mongo/src/test/scala/com/avsystem/commons/mongo/typed/MongoRefTest.scala).

**NOTE**: some of the above references (e.g. unwrapping optional fields or narrowing sealed hierarchies) introduce
an _implied filter_ that is automatically included into the query if this reference is used in a MongoDB query document
or projection.

### `MongoDocumentFilter`

A `MongoDocumentFilter[E]` represents a MongoDB query document for an entity `E`. Usually, filters are created
via `MognoPropertyRef`s, e.g. 

```scala
val query: MongoDocumentFilter[MyEntity] = MyEntity.ref(_.int) > 10 && MyEntity.ref(_.data.flag).is(true)
```

For more examples, see the Scaladoc or 
[tests](https://github.com/AVSystem/scala-commons/blob/mongo-api/commons-mongo/src/test/scala/com/avsystem/commons/mongo/typed/MongoFilterTest.scala).

### `MongoProjection`

A `MongoProjection[E]` represents a MongoDB projection document that specifies the properties that should be included
in the response of a query. It is usually one of:

* The whole-document projection, e.g. `MyEntity.SelfRef`.
* The whole-document projection narrowed to a single case class or intermediate subtrait in a sealed hierarchy, e.g.
  `MyUnionEntity.as[FirstCase]`.
* A single `MongoPropertyRef`, e.g. `MyEntity.ref(_.int)`
* An arbitrary combination of projections into a tuple, e.g. `MongoProjection.zip(MyEntity.ref(_.int), MyEntity.ref(_.data))`

For more examples, see the Scaladoc or
[tests](https://github.com/AVSystem/scala-commons/blob/mongo-api/commons-mongo/src/test/scala/com/avsystem/commons/mongo/typed/MongoProjectionTest.scala).

### `MongoDocumentOrder`

A `MongoDocumentOrder[E]` represents a sort order for a document of type `E`. It may be one of:

* Unspecified (empty) order, i.e. `MongoDocumentOrder.unspecified`
* Single-field order, e.g. `MyEntity.ref(_.int).ascending`
* Multi-field order, e.g. `MongoDocumentOrder(MyEntity.ref(_.int) -> true, MyEntity.ref(_.data.number) -> false)`

For more examples, see the Scaladoc or
[tests](https://github.com/AVSystem/scala-commons/blob/mongo-api/commons-mongo/src/test/scala/com/avsystem/commons/mongo/typed/MongoOrderTest.scala).

### `MongoDocumentUpdate`

A `MongoDocumentUpdate[E]` represents a MongoDB update document for an entity of type `E`. Example:

```scala
val update: MongoDocumentUpdate[MyEntity] = 
  MyEntity.ref(_.strOpt).set(Opt("foo")) && MyEntity.ref(_.int).inc(5)
```

For more examples, see the Scaladoc or
[tests](https://github.com/AVSystem/scala-commons/blob/mongo-api/commons-mongo/src/test/scala/com/avsystem/commons/mongo/typed/MongoUpdateTest.scala).

### `MongoIndex[E]`

A `MongoIndex[E]` represents a MongoDB index document for an entity of type `E`. Examples:

* Single field index, e.g. `MyEntity.ref(_.int).ascendingIndex`
* Multi-field index, e.g.

  ```scala
  import MongoIndexType._
  val index: MongoIndex[MyEntity] =
  MongoIndex(MyEntity.ref(_.str) -> Hashed, MyEntity.ref(_.int) -> Descending)
  ```

## Invoking database commands

