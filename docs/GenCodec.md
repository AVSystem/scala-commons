# `GenCodec`

`GenCodec` is a **serialization library** within AVSystem's `scala-commons` library.

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [`GenCodec`](#gencodec)
  - [The `GenCodec` typeclass](#the-gencodec-typeclass)
    - [Formats supported by default](#formats-supported-by-default)
    - [`GenKeyCodec`](#genkeycodec)
    - [`GenObjectCodec`](#genobjectcodec)
  - [Writing and reading](#writing-and-reading)
  - [`GenCodec` instances available by default](#gencodec-instances-available-by-default)
  - [Deriving codecs](#deriving-codecs)
    - [Deriving codecs for generic types](#deriving-codecs-for-generic-types)
    - [Depending on external implicits](#depending-on-external-implicits)
  - [Serializing case classes](#serializing-case-classes)
    - [Field name customization](#field-name-customization)
    - [Default field values](#default-field-values)
      - [Transient default field values](#transient-default-field-values)
    - [Optional and nullable fields](#optional-and-nullable-fields)
    - [Case class like types](#case-class-like-types)
  - [Serializing sealed hierarchies](#serializing-sealed-hierarchies)
    - [Nested sealed hierarchy format](#nested-sealed-hierarchy-format)
    - [Flat sealed hierarchy format](#flat-sealed-hierarchy-format)
      - [Sealed hierarchy default case](#sealed-hierarchy-default-case)
    - [Case name customization](#case-name-customization)
  - [Transparent wrappers](#transparent-wrappers)
  - [Writing codecs for third party types](#writing-codecs-for-third-party-types)
    - [Derive the codec from a "fake companion"](#derive-the-codec-from-a-fake-companion)
    - [Transform the codec of another type](#transform-the-codec-of-another-type)
    - [Implement the codec manually](#implement-the-codec-manually)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

Like most Scala serialization libraries, `GenCodec`
leverages [typeclasses](https://scalac.io/blog/typeclasses-in-scala/). Typeclass instances can be automatically
generated for algebraic data types (case classes and sealed hierarchies) in compilation time. This provides almost zero
boilerplate and does not require any usage of runtime reflection, which has several benefits:

* platform independence - `GenCodec` works in Scala-JVM as well as Scaja.js (and possibly Scala Native in the future)
* performance - statically generated code is faster than runtime reflection
* precision - codec generation can access full type information, not limited by type erasure
* type safety - codec generation thoroughly validates types and issues compilation errors when something is wrong (e.g.
  when a codec instance is missing for some case class field type)

[Old Documentation](GenCodecOld.md) (with more internals)

## The `GenCodec` typeclass

The `GenCodec` typeclass itself is defined as:

```scala
import com.avsystem.commons.serialization._

trait GenCodec[T] {
  def read(input: Input): T
  def write(output: Output, value: T): Unit
}
```

`GenCodec` is (as its name suggests) generic, in the sense that it is not bound to a single serialization format like
JSON. Instead, it uses more abstract `Input` and `Output` traits for reading and writing the actual raw serialized data.
This raw data may be a `String`, a byte sequence, some intermediate format like JSON AST or pretty much anything else.
This is determined fully by the implementation of `Input` and `Output` traits.

However, `Input` and `Output` define a common denominator that must be supported by all the serialization formats.
Namely, they must all have a JSON-like structure, i.e.

* they must support writing and reading primitive types like `String`, `Boolean`, `Int`, `Double`, `BigInteger`,
  `BigDecimal`, `Array[Byte]`, `Timestamp`.
  Note that **not** all these types must have a native, unambiguous
  representation in the target format. For example, a `Timestamp` can be represented as an ISO string or a raw number of
  milliseconds.
* they must support writing a `null` value and checking for `null` when reading
* they must support writing and reading _lists_ - sequences of arbitrary values
* they must support writing and reading _objects_ - mappings of string keys to arbitrary values

### Formats supported by default

Within `scala-commons` you can find `Input` and `Output` implementations for the following serialization formats:

* [JSON](https://www.json.org/json-en.html) represented as raw strings - `JsonStringInput` & `JsonStringOutput`
* [CBOR](https://cbor.io/) represented as raw byte arrays - `CborInput` & `CborOutput`
  (see [detailed documentation](CBOR.md) on CBOR support)
* [BSON](https://bsonspec.org/) - available in `commons-mongo` module
    * using Java intermediate `BsonValue` representation - `BsonValueInput` & `BsonValueOutput`
    * using Java stream-like `BsonReader` and `BsonWriter` - `BsonReaderInput` & `BsonWriterOutput`
* [HOCON](https://github.com/lightbend/config/blob/main/HOCON.md) using
  [Lightbend Config](https://github.com/lightbend/config) representation, available in `commons-hocon` module - 
  `HoconInput` & `HoconOutput`

Also, in principle it should be relatively easy to implement `Input` and `Output` for various intermediate representations 
found in third party libraries, e.g. JSON ASTs implemented by all the JSON serialization libraries.

### `GenKeyCodec`

`GenKeyCodec` is an auxiliary typeclass to `GenCodec`. It defines conversion from and to a string key. These are usually
used as object keys by a `GenCodec` instance that writes an object.

### `GenObjectCodec`

`GenObjectCodec` is a subclass of `GenCodec` which guarantees that serialization produces an _object_ (a string key to
arbitrary value association).

## Writing and reading

Convenient methods to write and read values using `GenCodec` are usually provided by (companion objects of) `Input`
and `Output` implementations. For example, to serialize a list of numbers into a JSON string:

```scala
import com.avsystem.commons.serialization._
import com.avsystem.commons.serialization.json._

val jsonStr: String = JsonStringOutput.writePretty(List(1, 2, 3))
println(jsonStr) // [1, 2, 3]
```

Reading is similarly straightforward but (understandably) requires the value type to be given explicitly:

```scala
val ints: List[Int] = JsonStringInput.read[List[Int]]("[1, 2, 3]")
```

## `GenCodec` instances available by default

`GenCodec` comes with built-in instances for the following types:

* primitive types: `Unit`, `Boolean`, `Char`, `Byte`, `Short`, `Int`, `Long`, `Float`, `Double`
* boxed Java versions of primitive types, e.g. `java.lang.Boolean`, `java.lang.Integer`, etc.
* other simple types: `String`, `Array[Byte]`, `com.avsystem.commons.misc.Timestamp`, `java.util.Date`, `java.util.UUID`
* big numeric types: `BigInt`, `BigDecimal` and its Java counterparts
* special types: `Null`, `Nothing`
* Scala collections extending `scala.collection.Seq[T]` or `scala.collection.Set[T]` - assuming availability
  of `GenCodec[T]`
* Java collections extending `java.util.Collection[T]` - assuming availability of `GenCodec[T]`
* Scala maps extending `scala.collection.Map[K, V]` - assuming availability of `GenKeyCodec[K]` and `GenCodec[V]`
* Java maps extending `java.util.Map[K, V]` - assuming availability of `GenKeyCodec[K]` and `GenCodec[V]`
* Scala enums (extending `NamedEnum` with companion extending `NamedEnumCompanion`) - they serialize as strings equal to
  their names
* Java enums - they serialize as strings equal to their names
* `Option[T]`, `Opt[T]`, `OptArg[T]`, `NOpt[T]` - assuming availability of `GenCodec[T]`
    * empty values (`None`, `Opt.Empty`, etc) serialize as `null` while non-empty values serialize as-is - therefore it
      is not possible to unambiguously serialize `Some(null)` and `NOpt(null)` - they will collapse to `None`
      and `NOpt.Empty` upon deserialization (note that `Opt(null)` and `OptArg(null)` already collapse to empty values
      in runtime, independent of serialization).
* `Either[A, B]` - assuming availability of `GenCodec[A]` and `GenCodec[B]`

All collections are serialized into lists (arrays) while all maps are serialized into objects.

## Deriving codecs

In order to serialize your own types, you must provide `GenCodec` instances independently. Fortunately, most of the
time serialized types are algebraic data types (case classes and sealed hierarchies) for which there are convenient
macros for codec generation.

The easiest way to provide a `GenCodec` instance for a case class or sealed hierarchy is to use `HasGenCodec` as a base
class for its companion object, e.g.

```scala
import com.avsystem.commons.serialization._

case class Data(int: Int, string: String)
object Data extends HasGenCodec[Data]
```

This is a shorter version of a more general way:

```scala
case class Data(int: Int, string: String)
object Data {
  implicit val codec: GenCodec[Data] = GenCodec.materialize
}
```

### Deriving codecs for generic types

Usage of `HasGenCodec` is limited to non-generic types. If your type has exactly one type parameter, you can
use `HasPolyGenCodec` instead:

```scala
import com.avsystem.commons.serialization._

case class Point[T](x: T, y: T)
object Point extends HasPolyGenCodec[T]
```

which is roughly equivalent to:

```scala
import com.avsystem.commons.serialization._

case class Point[T](x: T, y: T)
object Point {
  implicit def codec[T: GenCodec]: GenCodec[Point[T]] = GenCodec.materialize
}
```

There's also a similar convenience companion for GADTs (generic algebraic data types):

```scala
sealed trait Expr[T]
case object NullExpr extends Expr[Null]
case class IntExpr(value: Int) extends Expr[Int]
case class StrExpr(str: String) extends Expr[String]
object Expr extends HasGadtCodec[Expr]
```

There are infinitely many ways (_kinds_) by which your data types may be generic. You may have two, three or more type
parameters, they may have additional bounds, require additional implicits, etc. It is impossible to cover all these
possibilities with a finite set of base companion classes like `HasGenCodec`. However, you can always fall back to
declaring the codec explicitly and using `GenCodec.materialize`. It is also relatively easy
to make your own base companion class, similar to `HasGenCodec` that can cover your particular generic scenario.

### Depending on external implicits

When generating a `GenCodec` instance for a case class, the macro requires that all the field types already have
a `GenCodec` on their own. Sometimes it's necessary to import additional implicits in order to fulfill this requirement.
In order to avoid importing manually you can put your additional implicits into an object and use `HasGenCodecWithDeps`.
For example:

```scala
import com.avsystem.commons.serialization._

type ThirdPartyType // defined in a library

object AdditionalImplicits {
  implicit val thirdPartyTypeCodec: GenCodec[ThirdPartyType] = ???
}

case class Data(int: Int, thirdParty: ThirdPartyType)
object Data extends HasGenCodecWithDeps[AdditionalImplicits.type, Data]
```

This is roughly equivalent to:

```scala
case class Data(int: Int, thirdParty: ThirdPartyType)
object Data {
  implicit val codec: GenCodec[Data] = {
    import AdditionalImplicits._
    GenCodec.materialize[Data]
  }
}
```

In order to further reduce boilerplate, it may be worth introducing your own version of `HasGenCodec` that has these
implicits baked in:

```scala
object AdditionalImplicits {
  // ...
}

abstract class BetterHasGenCodec[T](implicit
  instances: MacroInstances[AdditionalImplicits.type, () => GenCodec[T]]
) extends HasGenCodecWithDeps[AdditionalImplicits.type]
```

(do not worry about the `MacroInstances` thing - or see its documentation if you're interested in details)

## Serializing case classes

Case classes serialize into objects. For example:

```scala
import com.avsystem.commons.serialization._

case class Data(int: Int, string: String)
object Data extends HasGenCodec[Data]

println(JsonStringOutput.write(Data(42, "foo"))) // {"int":42,"string":"foo"}
```

Fields are always written in the order of their declaration. However, when deserializing the order does **not** need to
be preserved - the codec will read fields in any order. Also, not all `Output`/`Input` implementations preserve field
order. This allows you to freely change the order of case class fields without breaking serialization compatibility.

### Field name customization

Using the `@name` annotation, it is possible to change serialized field names:

```scala
import com.avsystem.commons.serialization._

case class Data(int: Int, @name("str") string: String)
object Data extends HasGenCodec[Data]

println(JsonStringOutput.write(Data(42, "foo"))) // {"int":42,"str":"foo"}
```

### Default field values

If you declare a default value for a case class field, the codec will use this value during deserialization in case it
is missing:

```scala
import com.avsystem.commons.serialization._

case class Data(int: Int, string: String = "default")
object Data extends HasGenCodec[Data]

println(JsonStringInput.read[Data]("""{"int":42}""")) // Data(42, "default")
```

You can achieve the same with `@whenAbsent` annotation if you want the default value to be used **only** during
deserialization (i.e. you don't want a language-level default value):

```scala
case class Data(int: Int, @whenAbsent("default") string: String)
```

Default field values allow you to evolve your case classes by adding fields, without breaking serialization compatibility.

#### Transient default field values

You can also tell the macro-generated codec to omit the default value when serializing. This is done
with `@transientDefault` annotation:

```scala
import com.avsystem.commons.serialization._

case class Data(int: Int, @transientDefault string: String = "default")
object Data extends HasGenCodec[Data]

println(JsonStringOutput.write(Data(42))) // {"int":42}
```

### Optional and nullable fields

By default, fields whose type is an `Option`, `Opt` etc. are not treated in any special way. This means that
`None`, `Opt.Empty` etc. will be serialized into `null`:

```scala
import com.avsystem.commons.serialization._

case class Data(int: Int, str: Option[String])
object Data extends HasGenCodec[Data]

println(JsonStringOutput.write(Data(42, None))) // {"int":42,"str":null}
```

If you want to get rid of these `null`-valued fields and make the `Option`/`Opt` effectively represent
_complete absence_ of a field, use `@optionalParam` annotation:

```scala
import com.avsystem.commons.serialization._

case class Data(int: Int, @optionalParam str: Option[String])
object Data extends HasGenCodec[Data]

println(JsonStringOutput.write(Data(42, None))) // {"int":42}
```

Technically, the same can be achieved using default values and `@transientDefault`:

```scala
case class Data(int: Int, @transientDefault str: Option[String] = None)
```

However, `@optionalParam` is the recommended, more "native" way to do this.

When using `@optionalParam` with `Option`/`Opt`/`OptArg`, `null`-valued fields are treated equivalently to missing fields. If you
need to distinguish between missing fields and `null`-valued fields, this can be achieved with the help of `NOpt` (a
nullable `Opt`):

```scala
import com.avsystem.commons.serialization._

case class Data(int: Int, @optionalParam str: NOpt[Option[String]])
object Data extends HasGenCodec[Data]

def printJson(value: Data): Unit =
  println(JsonStringOutput.write(value))

printJson(Data(42, NOpt.Empty))        // {"int":42}
printJson(Data(42, NOpt(None)))        // {"int":42,"str":null}
printJson(Data(42, NOpt(Some("foo")))) // {"int":42,"str":"foo"}
```

### Case class like types

The macro that materializes codecs for case classes does not strictly require a `case class`. It is enough if
a class or trait _looks sufficiently like_ a case class. Strictly speaking, the class or trait must have a companion
object with `apply` and `unapply` methods defined as if it were a case class (`unapplySeq` if repeated parameters are 
in play). For a real `case class`, these methods are automatically synthesized by the compiler.

```scala
import com.avsystem.commons.serialization._

trait Stuff {
  def intValue: Int
  def strValue: String
}
object Stuff extends HasGenCodec[Stuff] {
  def apply(int: Int, str: String): Stuff =
    new Stuff {
      def intValue = int
      def strValue = str
    }
    
  def unapply(stuff: Stuff): Some[(Int, String)] =
    Some((stuff.intValue, stuff.strValue))
}

println(JsonStringOutput.write(Stuff(42, "foo"))) // {"int":42,"str":"foo"}
```

## Serializing sealed hierarchies

Sealed hierarchies are serialized as objects. There are two formats available: _nested_ and _flat_ (discriminator
based):

### Nested sealed hierarchy format

The _nested_ sealed hierarchy format is the default one. In this format, each case class/object representation is
wrapped into a single-field object. Name of this sole field is the name of the case class/object.

```scala
import com.avsystem.commons.serialization._

sealed trait Expr
case class IntExpr(value: Int) extends Expr
case class StrExpr(value: String) extends Expr
case object NullExpr extends Expr
object Expr extends HasGenCodec[Expr]

def printJson(value: Expr): Unit =
  println(JsonStringOutput.write[Expr](value))

printJson(IntExpr(42))    // {"IntExpr":{"value":42}}
printJson(StrExpr("foo")) // {"StrExpr":{"value":"foo"}}
printJson(NullExpr)       // {"NullExpr":{}}
```

The advantage of this format is that case classes and objects don't need to serialize into objects.
For example, single-field case classes can be made "transparent" using `@transparent` annotation, effectively
serializing them into the same value as their single field.

```scala
import com.avsystem.commons.serialization._

sealed trait Expr
@transparent case class IntExpr(value: Int) extends Expr
@transparent case class StrExpr(value: String) extends Expr
case object NullExpr extends Expr
object Expr extends HasGenCodec[Expr]

def printJson(value: Expr): Unit =
  println(JsonStringOutput.write[Expr](value))

printJson(IntExpr(42))    // {"IntExpr":42}
printJson(StrExpr("foo")) // {"StrExpr":"foo"}
printJson(NullExpr)       // {"NullExpr":{}}
```

### Flat sealed hierarchy format

Flat sealed hierarchy format uses _discriminator_ field instead of nested objects.
In order to enable the flat format, use `@flatten` annotation on the sealed trait.
As an argument it accepts optional discriminator field name (the default is `"_case"`).

```scala
import com.avsystem.commons.serialization._

@flatten("type") sealed trait Expr
case class IntExpr(value: Int) extends Expr
case class StrExpr(value: String) extends Expr
case object NullExpr extends Expr
object Expr extends HasGenCodec[Expr]

def printJson(value: Expr): Unit =
  println(JsonStringOutput.write[Expr](value))

printJson(IntExpr(42))    // {"type":"IntExpr","value":42}
printJson(StrExpr("foo")) // {"type":"StrExpr","value":"foo"}
printJson(NullExpr)       // {"type":"NullExpr"}
```

Flat sealed hierarchy format is cleaner but requires that all case classes and objects serialize into objects.
It may also be sensitive to object field order because the discriminator field must be read before any other fields
(this is only a problem with `Input` implementations that do not support random field access by name -
`JsonStringInput` does not have this problem).

#### Sealed hierarchy default case

When using the flat format, one of the case classes/objects may be annotated as `@defaultCase`. This allows the
discriminator field to be missing during deserialization of this particular case.

```scala
import com.avsystem.commons.serialization._

@flatten("type") sealed trait Expr
@defaultCase case class IntExpr(value: Int) extends Expr
case class StrExpr(value: String) extends Expr
case object NullExpr extends Expr
object Expr extends HasGenCodec[Expr]

println(JsonStringInput.read[Expr]("""{"value":42}""")) // IntExpr(42)
```

This is useful when evolving your data types - you can refactor a standalone case class into a sealed hierarchy
without breaking serialization compatibility.

### Case name customization

In either the nested or flat format, you can apply `@name` annotation on your case classes or objects in order to
customize their serialized names (outer object field name in nested format or discriminator field value in flat format):

```scala
import com.avsystem.commons.serialization._

@flatten("type") sealed trait Expr
@name("int") case class IntExpr(value: Int) extends Expr
@name("str") case class StrExpr(value: String) extends Expr
@name("null") case object NullExpr extends Expr
object Expr extends HasGenCodec[Expr]

printJson(IntExpr(42))    // {"type":"int","value":42}
printJson(StrExpr("foo")) // {"type":"str","value":"foo"}
printJson(NullExpr)       // {"type":"null"}
```

## Transparent wrappers

If your type is a _transparent wrapper_, it will automatically have a `GenCodec` instance that uses the representation
of the wrapped type:

```scala
case class UserId(value: Int) extends AnyVal
object UserId extends IntWrapperCompanion[UserId]

println(JsonStringOutput.write(UserId(42))) // 42
```

The same can be achieved with `@transparent` annotation:

```scala
@transparent case class UserId(value: Int) extends AnyVal
object UserId extends HasGenCodec[UserId]
```

It is recommended to use `TransparentWrapperCompanion` (or one of its subclasses e.g. `IntWrapperCompanion`)
because it derives more typeclass instances from the wrapped type than just `GenCodec` (e.g. `GenKeyCodec`).

## Writing codecs for third party types

If you need a `GenCodec` instance for a type that you don't control (e.g. a library type) then you cannot put that
codec into the companion object of that type. Instead, it is usually placed into some object that can be later
[injected as additional implicits](#depending-on-external-implicits) when deriving other codecs.

To actually implement a `GenCodec` instance for a third party type, there are three options:

### Derive the codec from a "fake companion"

Create an object that imitates the companion of your third party type. This object should implement
`apply` and `unapply`/`unapplySeq` methods as if it were a companion object of a case class.
Then, you can derive a codec from it using `GenCodec.fromApplyUnapplyProvider`.

```scala
import com.avsystem.commons.serialization._
import java.time.Duration

object ThirdPartyCodecs {
  object JavaDurationAU {
    def apply(seconds: Long, nanos: Int): Duration = 
      Duration.ofSeconds(seconds).withNanos(nanos)
    def unapply(duration: Duration): Some[(Long, Int)] = 
      Some((duration.getSeconds, duration.getNano))
  }
  
  implicit val durationCodec: GenCodec[Duration] =
    GenCodec.fromApplyUnapplyProvider[Duration](JavaDurationAU)
}

import ThirdPartyCodecs._
println(JsonStringOutput.write(Duration.ofSeconds(5).withNanos(500))) // {"seconds":5,"nanos":500}
```

This method is limited to situations where you want to serialize a third party type into an object.

### Transform the codec of another type

Another relatively easy way of getting a codec for a third party type is by providing a bidirectional conversion
between that type and some type that already has a codec.

```scala
import com.avsystem.commons.serialization._
import java.time.Duration

object ThirdPartyCodecs {
  case class DurationRepr(seconds: Long, nanos: Int)
  object DurationRepr extends HasGenCodec[Duration]
  
  implicit val durationCodec: GenCodec[Duration] =
    DurationRepr.codec.transform[Duration](
      d => DurationRepr(d.getSeconds, d.getNamo), 
      dr => Duration.ofSeconds(dr.seconds).withNanos(dr.nanos)
    )
}

import ThirdPartyCodecs._
println(JsonStringOutput.write(Duration.ofSeconds(5).withNanos(500))) // {"seconds":5,"nanos":500}
```

This method is not limited to codecs that produce an object but the intermediate representation may incur some
performance overhead.

### Implement the codec manually

This is a last-resort option because writing codecs by hand can be tricky - primarily due to the fact that `Input`
and `Output` objects are impure and unsafe - you need to be very careful to use them properly and not break their
protocol (which is largely not validated with static types).

```scala
import com.avsystem.commons.serialization._
import java.time.Duration

object ThirdPartyCodecs {
  implicit val durationCodec: GenCodec[Duration] = new GenCodec.ObjectCodec[Duration] {
    def read(input: ObjectInput): Duration = {
      // this implementation requires that random field access is available or field order is strictly preserved
      val seconds = input.getNextNamedField("seconds").readSimple().readLong()
      val nanos = input.getNextNamedField("nanos").readSimple().readInt()
      Duration.ofSeconds(seconds).withNanos(nanos)
    }
    
    def write(output: ObjectOutput, value: Duration): Unit = {
      output.writeField("seconds").writeSimple().writeLong(value.getSeconds)
      output.writeField("nanos").writeSimple().writeInt(value.getNano)
    }
  }
}

import ThirdPartyCodecs._
println(JsonStringOutput.write(Duration.ofSeconds(5).withNanos(500))) // {"seconds":5,"nanos":500}
```

