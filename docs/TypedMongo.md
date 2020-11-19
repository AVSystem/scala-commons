# MongoDB API

The `commons-mongo` module contains various utilities that enhance standard Java & Scala drivers for MongoDB.
This documentation focuses on the `com.avsystem.commons.mongo.typed` package that contains a Scala-idiomatic and
typesafe layer over the Reactive Streams driver for MongoDB.

The core class of this typed API is [`TypedMongoCollection`](#invoking-database-commands) - 
a wrapper over Reactive Streams driver `MongoCollection` with more precisely typed and Scala-idiomatic API.

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [Quickstart](#quickstart)
  - [Defining an entity](#defining-an-entity)
  - [Setting up the client](#setting-up-the-client)
  - [Inserting documents](#inserting-documents)
  - [Finding documents by query](#finding-documents-by-query)
- [Modeling entities](#modeling-entities)
  - [Field types](#field-types)
  - [Embedded document types](#embedded-document-types)
  - [Optional fields](#optional-fields)
- [Core types](#core-types)
  - [`MongoPropertyRef`](#mongopropertyref)
  - [`MongoDocumentFilter`](#mongodocumentfilter)
  - [`MongoProjection`](#mongoprojection)
  - [`MongoDocumentOrder`](#mongodocumentorder)
  - [`MongoDocumentUpdate`](#mongodocumentupdate)
  - [`MongoIndex`](#mongoindex)
- [Invoking database commands](#invoking-database-commands)
  - [Monix `Task`](#monix-task)
  - [Monix `Observable`](#monix-observable)
  - [Blocking](#blocking)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Quickstart

### Defining an entity

```scala
import org.bson.types.ObjectId

case class SimpleEntity(
  id: ObjectId,
  int: Int,
  str: String
) extends MongoEntity[ObjectId]
object SimpleEntity extends MongoEntityCompanion[SimpleEntity]
```

### Setting up the client

```scala
import com.mongodb.reactivestreams.client.MongoClients

val client = MongoClients.create() // connects to localhost by default
val rawCollection = client.getDatabase("test").getCollection("myEntity")

val collection: TypedMongoCollection[SimpleEntity] = new TypedMongoCollection(rawCollection)
```

### Inserting documents

```scala
val entities = Seq(
  SimpleEntity(ObjectId.get(), 1, "first"),
  SimpleEntity(ObjectId.get(), 2, "second"),
  SimpleEntity(ObjectId.get(), 3, "third")
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

val observable: Observable[SimpleEntity] =
  collection.find(SimpleEntity.ref(_.int) > 1)

// use whatever Scheduler is appropriate in your context (like ExecutionContext for Futures)
import monix.execution.Scheduler.Implicits.global
// run the Observable
observable.foreach(entity => println(s"Found entity: $entity"))

// alternatively, collect all found entities into a List
val listTask: Task[List[SimpleEntity]] = observable.toListL
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
extend `MongoEntity` and its companion must extend `MongoDataCompanion` rather than `MongoEntityCompanion`.

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
  intList: List[Int],
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

`MongoPropertyRef` is a fundamental type that serves as an entry point for creating queries, projections, updates,
sort orders, indices, etc.

### `MongoDocumentFilter`

A `MongoDocumentFilter[E]` represents a MongoDB query document for an entity type `E`. Usually, filters are created
via `MongoPropertyRef`s, e.g. 

```scala
val query: MongoDocumentFilter[MyEntity] = 
  MyEntity.ref(_.int) > 10 && MyEntity.ref(_.data.flag).is(true)
```

For more examples, see the Scaladoc or 
[tests](https://github.com/AVSystem/scala-commons/blob/mongo-api/commons-mongo/src/test/scala/com/avsystem/commons/mongo/typed/MongoFilterTest.scala).

#### Unsupported operators

Not all MongoDB query operators are directly supported by this API. If you want to use some unsupported query operator,
you can use `.rawQueryOp`, e.g.

```scala
import org.bson._

val query: MongoDocumentFilter[MyEntity] = 
  MyEntity.ref(_.int).rawQueryOp("$bitsAllClear", new BsonInt32(0x3F))
```

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

### `MongoIndex`

A `MongoIndex[E]` represents a MongoDB index document for an entity of type `E`. Examples:

* Single field index, e.g. `MyEntity.ref(_.int).ascendingIndex`
* Compound (multi field) index, e.g.

  ```scala
  import MongoIndexType._
  val index: MongoIndex[MyEntity] =
  MongoIndex(MyEntity.ref(_.str) -> Hashed, MyEntity.ref(_.int) -> Descending)
  ```
  
For more examples, see the Scaladoc or
[tests](https://github.com/AVSystem/scala-commons/blob/mongo-api/commons-mongo/src/test/scala/com/avsystem/commons/mongo/typed/MongoIndexTest.scala).

## Invoking database commands

In order to invoke database commands, create a `TypedMongoCollection` which is a wrapper over Reactive Streams
driver's raw `MongoCollection`. 

(assuming sample entity classes from the [previous section](#core-types))

```scala
import com.mongodb.reactivestreams.client.MongoClients

val client = MongoClients.create() // connects to localhost by default
val rawCollection = client.getDatabase("test").getCollection("myEntity")

val collection: TypedMongoCollection[MyEntity] = new TypedMongoCollection(rawCollection)

def createEntity(i: Int): MyEntity =
  MyEntity(s"id$i", i, s"str$i", Some(s"optstr$i"), (i to 10).toList, Map.empty, MyData(i.toDouble, flag = true))

val program: Task[Unit] = for {
  _ <- collection.insertMany((0 until 10).map(createEntity))
  _ <- collection.updateMany(
    MyEntity.ref(_.int) > 5, 
    MyEntity.ref(_.int).inc(10) && MyEntity.ref(_.str).set("modified")
  )
  modifiedEntities <- collection.find(MyEntity.ref(_.str).is("modified")).toListL
} yield {
  println(s"Found entities: $modifiedEntities")
}

import monix.execution.Scheduler.Implicits.global
program.runToFuture
```

For more examples of database operations with `TypedMongoCollection`, see
[tests](https://github.com/AVSystem/scala-commons/blob/mongo-api/commons-mongo/src/test/scala/com/avsystem/commons/mongo/typed/TypedMongoCollectionTest.scala).

`TypedMongoCollection` exposes mostly the same operations as Reactive Streams `MongoCollection` but typed differently, 
that is: more precisely and in a more Scala-idiomatic way:

* instead of raw `Bson`s, `TypedMongoCollection` uses more precise `MongoDocumentFilter`, `MongoProjection`, 
  `MongoDocumentUpdate`, etc. for expressing queries, projections, updates, sort orders, indices, etc.
* instead of using raw Reactive Streams `Publisher`, `TypedMongoCollection` returns operation results either as
  a [Monix `Task`](https://monix.io/docs/current/eval/task.html) (for single-element results) or as
  an [Monix `Observable`](https://monix.io/docs/current/reactive/observable.html) (for multi-element results).

For a proper and complete guide and documentation on Monix, refer to its [website](https://monix.io/).
Here, we will outline the most important aspects that will let you quickly get started with the MongoDB API.

The raw Reactive Streams driver is problematic to use because `org.reactivestreams.Publisher` returned by 
`MongoCollection` lacks high-level, straightforward to use API. It also does not express very well the distinction
between methods that return single result and methods that return multiple results.

### [Monix `Task`](https://monix.io/docs/current/eval/task.html)

A `Task[T]` represents an asynchronous computation that yields a result of type `T`. In this sense, it is somewhat
similar to `Future[T]`. In particular, both share a lot of similar methods and can be used in Scala for comprehensions.

However, there is a very fundamental, conceptual difference between a `Task` and a `Future`:

* a `Future` represents a result of _already running or finished_ computation
* a `Task` represents an _unexecuted_ program that needs to be run explicitly (e.g. by calling `.runToFuture`) 
  and may be run multiple times or concurrently, each time repeating its side effects and potentially 
  yielding different results
  
```scala
import monix.execution.Scheduler.Implicits.global // note: Scheduler extends ExecutionContext

val future: Future[Unit] = Future(pritnln("hello")) // "hello" is printed immediately

val task: Task[Unit] = Task.eval(println("hello")) // nothing happens
task.runToFuture // "hello" is printed
task.runToFuture // "hello" is printed again, concurrently with the previous run
```
  
One of the consequences of the above is that `Task` is 
[referentially transparent](https://en.wikipedia.org/wiki/Referential_transparency) while `Future` 
[is not](https://stackoverflow.com/questions/44196088/why-future-has-side-effects).
For people acknowledged with (pure) functional programming this will be a virtue by itself. Here we can outline some
immediate practical benefits of that property.

One of the consequences of `Task`'s referential transparency that makes it much easier to work with than with `Future` 
is that we don't need an implicit `ExecutionContext` to invoke transformation methods like `map`, `flatMap`, etc. 
Instead, a `Scheduler` (extended `ExecutionContext`) is necessary but only at the very point where `Task` is being
executed and converted into a `Future` (among other options).

This means that we can compose our asynchronous programs (represented as `Task`s) out of smaller programs without
being bound to any particular `Scheduler`. This relieves us from constant dragging of an `ExecutionContext` via
implicit parameters that we have to do when working with `Future`s.

```scala
val fetchValue: Task[Int] = ...
def convertValue(int: Int): Task[String] = ...

// no ExecutionContext/Scheduler necessary at this point
val fullProgram: Task[Unit] = 
  for {
    value <- fetchValue
    converted <- convertValue(value)
  } yield {
    println(converted)
  }

// only now we need a Scheduler
import monix.execution.Scheduler.Implicits.global
val future: Future[Unit] = fullProgram.runToFuture
```

### [Monix `Observable`](https://monix.io/docs/current/reactive/observable.html)

An `Observable[T]` is conceptually the same thing as a Reactive Streams `Publisher[T]` (actually, it's fairly straightforward
to convert between each other). That is, both represent an asynchronous stream of values of type `T` with backpressure
(a consumer must explicitly request more elements from the producer). The difference is that `Observable` exposes a 
rich, Scala-idiomatic API with a lot of high-level combinators.

There are many ways to consume results of an `Observable[T]`. If you don't need the streaming nature of the `Observable`,
you can simply "degrade" it onto a `Task[List[T]]` by calling `.toListL` on it. This is very often used in MongoDB API
in order to fetch full results of a query into memory.

**NOTE**: You can think of `Observable` as an asynchronous version of `Iterable`.

### Blocking

It is recommended to use MongoDB API in a non-blocking way (by composing `Task`s and `Observables`s).
However, sometimes this is not possible - some external API may force us into synchronous, blocking implementations.
Then we have no choice but to wait for the database on the current JVM thread.

In order to do this, use utilities provided by `com.avsystem.commons.concurrent.BlockingUtils` class. Your project should
provide an implementation of this class where an appropriate Monix `Scheduler`s and other options (default timeout) 
will be configured, e.g.

```scala
import com.avsystem.commons.concurrent.BlockingUtils
import monix.execution.Scheduler

object Blocking extends BlockingUtils {
  // some Scheduler usually reused throughout your application
  def scheduler: Scheduler = Scheduler.global 
  // some Scheduler usually reused throughout your application (unbounded, for blocking code)
  def ioScheduler: Scheduler = Scheduler.io() 
}
```

Using this utility, you can:

* Await a `Future[T]` and get a `T` using `Blocking.await`
* Run and await a `Task[T]` and get a `T` using `Blocking.runAndAwait`
* Turn an `Observable[T]` into a `CloseableIterator[T]` using `Blocking.toIterator`

`BlockingUtils` was also designed to be easily invoked from Java. In particular, `CloseableIterator`
implements both Java & Scala `Iterator`.

### Unsupported operations

`TypedMongoCollection` does not cover the entire API of Reactive Streams driver `MongoCollection`.
For example, the [aggregation](https://docs.mongodb.com/manual/aggregation/) is currently not covered.
In order to use this missing API, you can fall back to using operations on native `MongoCollection`. 
The easiest way to do this is via `singleResultNativeOp` or `multiResultNativeOp` which invoke some native command
specified as lambda expression and translate the result (`Publisher`) into a `Task` or `Observable`.

An example of how to invoke `aggregate`:

```scala
val collection: TypedMongoCollection[MyEntity] = ???

import org.bson._
import com.avsystem.commons._ // for JList
import monix.reactive.Observable

val pipeline = JList(/* aggregation pipeline `Bson` */)
val results: Observable[Document] = collection.multiResultNativeOp(_.aggregate(pipeline))
```

## Relationship with the previous `commons-mongo` API

Before introducing `com.avsystem.commons.mongo.typed` package and `TypedMongoCollection`, the `commons-mongo` module
already had a relatively thin layer over various native drivers (Java synchronous, Java asynchronous, Java reactive, Scala).

This old API provides:

* `GenCodec` based serialization - four variants of `GenCodecCollection` creators for different native drivers
* `BsonRef` - references to document properties, superseded by `MongoPropertyRef` in the new API
* extension methods for raw `Bson` type for creating queries, updates, etc. - explicit imports are required

In comparison to the old API, the new one provides:

* `TypedMongoCollection` wrapper over Reactive Streams collection - as opposed to native `MongoCollection`s
  * other drivers are unsupported because they are deprecated, synchronous or redundant
* integration with Monix (`Task`s and `Observable`s as opposed to raw `Publisher`s)
* well typed query/projection/update/index documents in place of raw `Bson`
* more high-level and user-friendly API for creating queries/updates/etc than raw `Bson` building
* better support for sealed hierarchies
