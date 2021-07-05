# CBOR support

`commons-core` module comes with built-in implementation of [Concise Binary Object Representation](https://cbor.io/) via
`CborInput` and `CborOutput`. They can be used to serialize and deserialize any type that has a `GenCodec` instance.

However, plain `GenCodec` cannot use all the power of CBOR, requiring some additional support.

## Basic usage with `GenCodec`

Nothing else than a `GenCodec` instance is necessary to serialize a type into CBOR. For example:

```scala
import com.avsystem.commons.serialization._
import com.avsystem.commons.serialization.cbor._

case class Thing(int: Int, str: String)
object Thing extends HasGenCodec[Thing]

val cborBytes: Array[Byte] = CborOutput.write(Thing(42, "foo"))
```

### `RawCbor`

You can also serialize into `RawCbor`, a wrapper over `Array[Byte]` that provides sensible `equals`, `hashCode`,
`toString` and other utilities.

```scala
val rawCbor: RawCbor = CborOutput.writeRawCbor(Thing(42, "foo"))
```

`RawCbor` can also be used as a field type in a case class in order to represent arbitrary CBOR-serialized data:

```scala
case class RawCborThing(id: Int, data: RawCbor)
object RawCborThing extends HasGenCodec[RawCborThing]
```

## Explicitly sized collections

CBOR lists and maps can be encoded in two ways:

* with explicit number of elements at the beginning (more compact, allows preallocation of buffers and better
  performance)
* with unspecified number of elements at the beginning along with a _break_ byte (`0xFF`) at the end (better for streaming
  applications)

When serializing collections into CBOR, you can control whether they are explicitly sized. This is done with
`SizePolicy` that can be passed into `CborOutput`, e.g.

```scala
val cborList = CborOutput.write(List(1, 2, 3), sizePolicy = SizePolicy.Required)
```

`SizePolicy` has three values:

* `Required` - Collections are always written with explicit size in their CBOR representation. This may be slow for
  collections that don't have a fast `.size` method, e.g. `List`
* `Optional` - Only collections that have a fast `.size` method are written with explicit size, e.g. `Vector`. This is
  the default behavior.
* `Ignored` - All collections are written without explicit size

## Using CBOR keys of arbitrary type

`GenCodec` has a strong assumption that all object/map keys are `String`s, like in JSON. One of the consequences is that
serializing a `Map[K, V]` with `GenCodec` requires an instance of `GenKeyCodec` for `K` so that keys can be converted to
strings.

However, CBOR has no such limitations. Map keys can be arbitrary CBOR values. There are some additional tools
in `GenCodec` and `CborInput/Output` that can leverage this.

### `CborKeyCodec`

`CborInput.read` and `CborOutput.write` accept a `CborKeyCodec` parameter that defines translation between string-typed
keys and arbitrary CBOR-encoded keys. You can use this e.g. to give a numeric label to every textual field, reducing the
size of resulting CBOR.

```scala
val keyCodec = new CborKeyCodec {
  def writeFieldKey(fieldName: String, output: CborOutput): Unit = fieldName match {
    case "intField" => output.writeInt(1)
    case "strField" => output.writeInt(2)
    case name => output.writeString(name)
  }

  def readFieldKey(input: CborInput): String = input.readInitialByte().majorType match {
    case MajorType.Unsigned => input.readInt() match {
      case 1 => "intField"
      case 2 => "strField"
      case n => throw new ReadFailure(s"unknown CBOR field label: $n")
    }
    case _ => input.readString()
  }
}

case class Thing(intField: Int, strField: String)
object Thing extends HasGenCodec[Thing]

val cborBytes: Array[Byte] = CborOutput.write(Thing(42, "foo"), keyCodec)
```

This solution is nice when field names are often reused across multiple case classes that end up being serialized into
final CBOR. Other than that, this is somewhat clunky and does not give you precise control on a level of each case
class.

### `HasCborCodec` and raw CBOR keys

In place of `HasGenCodec`, you can use `HasCborCodec`. It generates an instance of `GenCodec` that is optimized for
CBOR, which means two things:

* if your case class contains a field typed as `Map[K, V]`, map keys will be serialized directly into CBOR rather than
  converted into strings with `GenKeyCodec` (which would happen for standard `GenCodec`).
* you can use `@cborKey` and `@cborDiscriminator` annotations in your case class & sealed hierarchy definitions

Example:

```scala
@cborDiscriminator(0) sealed trait UnionData
object UnionData extends HasCborCodec[UnionData] {
  @cborKey(1) case class Textual(@cborKey(1) txt: String) extends UnionData
  @cborKey(2) case class Numeric(@cborKey(true) num: Int) extends UnionData
  @cborKey(3) case class Mapping(@cborKey("m") map: Map[Int, String]) extends UnionData
}
```

Here are some examples of how `UnionData` serializes into CBOR (using [diagnostic notation](http://cbor.me)):

```
UnionData.Textual("foo")                            => {0: 1, 1: "foo"}
UnionData.Numeric(42)                               => {0: 2, true: 42}
UnionData.Mapping(Map(1 -> "one", 2 -> "two"))      => {0: 3, "m": {1: "one", 2: "two"}}
```

* the field with key `0` in each case class representation is the _discriminator_ specified with
  `@cborDiscriminator` annotation
* discriminator values (`1`, `2` and `3`) come from `@cborKey` annotations applied on each case class
* case class field keys (`1`, `true` and `"m"`) come from `@cborKey` annotations applied on case class fields
* `Map[Int, String]` has keys serialized as plain numbers rather than strings

**NOTE**: `@cborKey` and `@cborDiscriminator` annotations accept any value serializable with `GenCodec`
