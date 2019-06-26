package com.avsystem.commons
package serialization

import com.avsystem.commons.misc.{AbstractValueEnum, AbstractValueEnumCompanion, EnumCtx}
import com.avsystem.commons.serialization.GenCodec.ReadFailure

/**
  * Base trait for type markers identifying custom native types that particular `Input` and `Output`
  * implementations might want to support.
  */
trait TypeMarker[T]

/**
  * Base trait for metadata markers identifying custom native metadata information that particular `Input` and
  * `Output` implementations might want to support.
  * Example: [[com.avsystem.commons.serialization.json.JsonType JsonType]]
  */
trait InputMetadata[T]

/**
  * Represents an abstract sink to which a value may be serialized (written).
  * An `Output` instance should be assumed to be stateful. After calling any of the `write` methods, it MUST NOT be
  * reused. This means that `Output` instance can be used only to write a single value. However, if the value
  * to write is complex, one can use `writeList`/`writeSet` or `writeObject`/`writeMap`.
  */
trait Output extends Any {
  def writeNull(): Unit
  def writeSimple(): SimpleOutput
  def writeList(): ListOutput
  def writeObject(): ObjectOutput

  /**
    * Attempts to write some arbitrary custom "native" value that this output may or may not support.
    * The custom type is identified by an instance of `TypeMarker` which is usually an object (e.g. companion
    * object of the custom `T` type itself). This way `Input` and `Output` implementations may support other
    * native types than the ones supported by default by `Input` and `Output` interfaces.
    *
    * Codecs may use this method to optimize encoded format in case it it possible with particular `Output`
    * implementation. `GenCodec` may generally assume that if this method returns `true` then corresponding
    * `Input` will return a non-empty `Opt` from `readCustom` method.
    *
    * `false` returned by this method indicates that this output does not support this particular type.
    * In such situation the codec must fall back to some other strategy. If the native type is supported but there was
    * some error writing it then a `WriteFailure` should be thrown instead of returning `false`.
    */
  def writeCustom[T](typeMarker: TypeMarker[T], value: T): Boolean = false

  /**
    * Determines whether serialization format implemented by this `Output` preserves particular arbitrary
    * "metadata" which is identified by [[com.avsystem.commons.serialization.InputMetadata InputMetadata]]
    * which is usually an object (e.g. companion object of metadata value type `T`).
    *
    * An example of [[com.avsystem.commons.serialization.InputMetadata InputMetadata]] is
    * [[com.avsystem.commons.serialization.json.JsonType JsonType]] supported by
    * [[com.avsystem.commons.serialization.json.JsonStringOutput JsonStringOutput]].
    *
    * If this method returns `true` then codec may optimize its encoded format and assume that a corresponding
    * `Input` implementation will return a non-empty `Opt` from its `readMetadata` implementation when passed the
    * same [[com.avsystem.commons.serialization.InputMetadata InputMetadata]] identifier.
    * If this method returns `false` then this `Output` does not support this
    * medatata type and codec should fall back to some other serialization strategy.
    */
  def keepsMetadata(metadata: InputMetadata[_]): Boolean = false

  /**
    * This ugly workaround has been introduced when standard `Option` encoding changed from zero-or-one element list
    * encoding to unwrapped-or-null encoding which effectively disallowed serializing `null` and `Some(null)`.
    * If some `Output` implementation still wants to use the list encoding, it may do it by overriding this method
    * and returning `true`.
    */
  def legacyOptionEncoding: Boolean = false
}

/**
  * Represent an abstract sink for "primitive" values, i.e. ones that can be written as a whole with a simple
  * method call (as opposed to lists and objects). Simple values must NEVER be `null`. `Output.writeNull` must
  * be used instead to handle `null` values.
  */
trait SimpleOutput extends Any {
  /** Value written MUST NOT be `null` */
  def writeString(str: String): Unit
  def writeChar(char: Char): Unit = writeString(char.toString)
  def writeBoolean(boolean: Boolean): Unit
  def writeByte(byte: Byte): Unit = writeShort(byte)
  def writeShort(short: Short): Unit = writeInt(short)
  def writeInt(int: Int): Unit
  def writeLong(long: Long): Unit
  def writeTimestamp(millis: Long): Unit = writeLong(millis)
  def writeFloat(float: Float): Unit = writeDouble(float)
  def writeDouble(double: Double): Unit
  /** Value written MUST NOT be `null` */
  def writeBigInt(bigInt: BigInt): Unit
  /** Value written MUST NOT be `null` */
  def writeBigDecimal(bigDecimal: BigDecimal): Unit
  /** Value written MUST NOT be `null` */
  def writeBinary(binary: Array[Byte]): Unit
}

trait OutputAndSimpleOutput extends Any with Output with SimpleOutput {
  override final def writeSimple(): SimpleOutput = this
}

/**
  * Using `SizePolicy`, a [[SequentialOutput]] ([[ListOutput]] or [[ObjectOutput]]) may hint the codec whether it
  * makes use or requires explicit list or object size to be declared with [[SequentialOutput.declareSize]].
  */
final class SizePolicy(implicit enumCtx: EnumCtx) extends AbstractValueEnum
object SizePolicy extends AbstractValueEnumCompanion[SizePolicy] {
  /**
    * Indicates that the [[SequentialOutput]] implementation does not utilize explicitly declared size in any way.
    * This means that the codec may always omit the `declareSize` invocation.
    */
  final val Ignored: Value = new SizePolicy

  /**
    * Indicates that the [[SequentialOutput]] implementation is able to take advantage of explicitly declared size
    * (e.g. in order to preallocate some buffers with accurate size or use more compact representation) but it is still
    * able to work without size known upfront. With this policy, the codec may decide on its own whether it's worth
    * computing the size upfront. Typically it will do it only when that computation is cheap, e.g. for a Scala `Vector`
    * but omit it when it could degrade performance, e.g. for a Scala `List` which requires entire list traversal to
    * compute its size.
    */
  final val Optional: Value = new SizePolicy

  /**
    * Indicates that the [[SequentialOutput]] implementation always requires the codec to declare list or object size
    * explicitly. The codec is then obliged to call [[SequentialOutput.declareSize]] before writing any elements or
    * fields, regardless of the cost of computing that size.
    */
  final val Required: Value = new SizePolicy
}

/**
  * Base trait for outputs which allow writing of multiple values in sequence, i.e. [[ListOutput]] and [[ObjectOutput]].
  */
trait SequentialOutput extends Any {
  /**
    * Gives the output explicit information about the number of elements or fields that will be written to this
    * output by the codec. This method must be called at most once, before any elements or fields have been written.
    * The codec is then required to write exactly the declared number of elements or fields.
    * Whether the codec should or must call this method depends on [[sizePolicy]] and the cost of computing the size.
    */
  def declareSize(size: Int): Unit = ()

  /**
    * Provides information about whether this output makes use of list or object size explicitly declared with
    * [[declareSize]]. See [[SizePolicy]] for more details.
    */
  def sizePolicy: SizePolicy = SizePolicy.Optional

  /**
    * Indicates that all elements or fields in this [[com.avsystem.commons.serialization.SequentialOutput SequentialOutput]]
    * have been written. This method MUST always be called after list/object writing has been finished.
    */
  def finish(): Unit
}
/**
  * Represents an abstract sink for serialization of sequences of values. Any [[ListOutput]] instance
  * must be assumed to be stateful and used in strictly sequential manner. After all elements have been written,
  * `finish()` must be called to explicitly mark that the list is complete.
  */
trait ListOutput extends Any with SequentialOutput {
  /**
    * Returns an [[com.avsystem.commons.serialization.Output Output]] representing next element in this list.
    * This [[com.avsystem.commons.serialization.Output Output]] instance MUST be fully used
    * before calling [[writeElement]] next time. That means, one can NOT simultaneously use multiple instances of
    * [[com.avsystem.commons.serialization.Output Output]] returned by subsequent calls to this method.
    */
  def writeElement(): Output
}
/**
  * Represents an abstract sink for serialization of string-to-value mappings. Any [[ObjectOutput]] instance
  * must be assumed to be stateful and used in strictly sequential manner. After all key-value pairs have been
  * written, `finish()` must be called to explicitly mark that the object is complete.
  * <p/>
  * [[ObjectOutput]] MUST preserve information about the order in which fields are written.
  * [[ObjectInput]] is required to read fields in exactly the same order as [[ObjectOutput]] writes them.
  */
trait ObjectOutput extends Any with SequentialOutput {
  /**
    * Returns an [[com.avsystem.commons.serialization.Output Output]] representing value mapped to given string key.
    * This [[com.avsystem.commons.serialization.Output Output]] instance must be fully
    * used before calling [[writeField]] next time. That means, one can NOT simultaneously use multiple instances
    * of [[com.avsystem.commons.serialization.Output Output]] returned by subsequent calls to this method.
    */
  def writeField(key: String): Output
}

/**
  * Represents an abstract source from which a value may be deserialized (read).
  * Each of the `read` methods tries to read a value of specified type and may throw an exception
  * (usually [[com.avsystem.commons.serialization.GenCodec.ReadFailure ReadFailure]]) when reading is not successful.
  * <p/>
  * An `Input` value should be assumed to be stateful. If any of the `readX` methods have already been called,
  * the `Input` instance can no longer be used and MUST be discarded.
  * <p/>
  * In order to ignore the value kept in this `Input`, `skip()` MUST be called.
  * <p/>
  * In summary: every `Input` MUST be fully exhausted by either calling one of the `read` methods which returns
  * successful value or by calling `skip()`. Also, [[ListInput]] and [[ObjectInput]] instances returned from this
  * `Input` must also be fully exhausted on their own.
  */
trait Input extends Any {
  /**
    * Attempts to read `null` value from an `Input`. Returning `true` means that input instance contained a
    * `null` value. Its state should then be changed so that input can be considered "consumed"
    * (no other reads are possible on this instance). Returning `false` means that the input contains something else
    * than a `null` value. Its state must not change in this situation and it must be possible to call some other
    * read method on it.
    */
  def readNull(): Boolean

  def readSimple(): SimpleInput
  def readList(): ListInput
  def readObject(): ObjectInput

  /**
    * Attempts to read some arbitrary "metadata" about this input instance. Metadata is identified by
    * [[com.avsystem.commons.serialization.InputMetadata InputMetadata]] which is usually an object
    * (e.g. companion object of metadata value type `T`).
    * An example of [[com.avsystem.commons.serialization.InputMetadata InputMetadata]] is
    * [[com.avsystem.commons.serialization.json.JsonType JsonType]] supported by
    * [[com.avsystem.commons.serialization.json.JsonStringInput JsonStringInput]].
    *
    * Codecs may use this method to optimize encoded format in case it it possible with particular `Input`
    * implementation. `GenCodec` may generally assume that if the data was written by a corresponding `Output`
    * that preserves particular metadata type (which may be determined by `Output.keepsMetadata()`) then
    * `readMetadata` will return a non-empty value.
    *
    * `Opt.Empty` may be returned form this method ONLY if this `Input` implementation does not support
    * this metadata type AT ALL. Any errors should be signaled by throwing `ReadFailure`.
    */
  def readMetadata[T](metadata: InputMetadata[T]): Opt[T] = Opt.Empty

  /**
    * Attempts to read some arbitrary custom "native" value that this input may or may not support.
    * The custom type is identified by an instance of `TypeMarker` which is usually an object (e.g. companion
    * object of the custom `T` type itself). This way `Input` and `Output` implementations may support other
    * native types than the ones supported by default by `Input` and `Output` interfaces.
    *
    * Codecs may use this method to optimize encoded format in case it it possible with particular `Input`
    * implementation. `GenCodec` may generally assume that if the data was written by a corresponding `Output`
    * which also support this custom native type then `readCustom` should return non-empty value.
    *
    * `Opt.Empty` returned by this method indicates that this input does not support this particular type.
    * If it supports it but there was some error reading it then a `ReadFailure` should be thrown instead of
    * returning `Opt.Empty`.
    */
  def readCustom[T](typeMarker: TypeMarker[T]): Opt[T] = Opt.Empty

  /** Ignores this input and skips its contents internally, if necessary */
  def skip(): Unit

  /**
    * This ugly workaround has been introduced when standard `Option` encoding changed from zero-or-one element list
    * encoding to unwrapped-or-null encoding which effectively disallowed serializing `null` and `Some(null)`.
    * If some `Input` implementation still wants to use the list encoding, it may do it by overriding this method
    * and returning `true`.
    */
  def legacyOptionEncoding: Boolean = false
}

/**
  * Represents an abstract source of primitive (or "simple") values. May be obtained from `Input`.
  */
trait SimpleInput extends Any {
  def readString(): String
  def readChar(): Char = readString().charAt(0)
  def readBoolean(): Boolean
  def readByte(): Byte = readShort().toByte
  def readShort(): Short = readInt().toShort
  def readInt(): Int
  def readLong(): Long
  def readTimestamp(): Long = readLong()
  def readFloat(): Float = readDouble().toFloat
  def readDouble(): Double
  def readBigInt(): BigInt
  def readBigDecimal(): BigDecimal
  def readBinary(): Array[Byte]
}

trait InputAndSimpleInput extends Any with Input with SimpleInput {
  override final def readSimple(): SimpleInput = this
}

trait SequentialInput extends Any {
  /**
    * Returns total number of elements or fields in this input or -1 if it is unknown.
    * This method can be used by codecs in order to optimize decoding of collections.
    */
  def knownSize: Int = -1

  def hasNext: Boolean
  def skipRemaining(): Unit
}
/**
  * Represents an abstract source of sequence of values that can be deserialized.
  * [[ListInput]] instance is stateful and MUST be read strictly sequentially. This means, you MUST fully exhaust
  * an `Input` instance returned by `nextElement()` before calling `nextElement()` again. For this reason,
  * [[ListInput]] is not an `Iterator` despite having similar interface
  * (`Iterator` would easily allow e.g. conversion to `List[Input]` which would be illegal).
  * <p/>
  * [[ListInput]] MUST always be fully exhausted. In order to ignore any remaining elements, skipRemaining() may be
  * used.
  */
trait ListInput extends Any with SequentialInput { self =>
  /**
    * Returns an [[com.avsystem.commons.serialization.Input Input]] representing next element in a sequence of values
    * represented by this [[com.avsystem.commons.serialization.ListInput ListInput]].
    * Returned [[com.avsystem.commons.serialization.Input Input]] instance must be fully exhausted before calling
    * `nextElement()` next time.
    */
  def nextElement(): Input

  def skipRemaining(): Unit = while (hasNext) nextElement().skip()
  def iterator[A](readFun: Input => A): Iterator[A] =
    new Iterator[A] {
      def hasNext: Boolean = self.hasNext
      def next(): A = readFun(nextElement())
    }
}
/**
  * Represents an abstract source of key-value mappings that can be deserialized.
  * [[com.avsystem.commons.serialization.ObjectInput ObjectInput]] instance is stateful and MUST be read strictly
  * sequentially. This means, you MUST fully exhaust any [[com.avsystem.commons.serialization.Input Input]] instance
  * returned by `nextField()` before calling `nextField()` again. For this reason,
  * [[com.avsystem.commons.serialization.ObjectInput ObjectInput]] is not an `Iterator` despite having similar interface
  * (`Iterator` would easily allow e.g. conversion to `List[(String, Input)]` which would be illegal).
  * <p/>
  * [[com.avsystem.commons.serialization.ObjectInput ObjectInput]] MUST always be fully exhausted.
  * In order to ignore any remaining key-value mappings, `skipRemaining()` may be used.
  * <p/>
  */
trait ObjectInput extends Any with SequentialInput { self =>
  /**
    * Returns [[com.avsystem.commons.serialization.FieldInput FieldInput]] that represents next field of this object.
    * You MUST NOT call `nextField()` again until this [[com.avsystem.commons.serialization.FieldInput FieldInput]]
    * is fully read or skipped.
    * </p>
    * Serialization format implemented by this `ObjectInput` must either preserve order of fields (as they are
    * written by corresponding `ObjectOutput`) OR it must provide random field access capability.
    * <ul>
    * <li>If the serialization format is able to preserve object field order then [[nextField]] must return
    * object fields in exactly the same order as they were written by `ObjectOutput.writeField`. This is
    * natural for most serialization formats backed by strings, raw character or byte sequences, e.g.
    * JSON implemented by [[com.avsystem.commons.serialization.json.JsonStringOutput JsonStringOutput]]/
    * [[com.avsystem.commons.serialization.json.JsonStringInput JsonStringInput]].</li>
    * <li>If the serialization format is unable to preserve object field order (e.g. because it uses hash maps to
    * represent objects) then it must instead support random, by-name field access by overriding [[peekField]].
    * </li>
    * </ul>
    */
  def nextField(): FieldInput

  /**
    * If serialization format implemented by `ObjectInput` does NOT preserve field order, then this method MUST
    * be overridden to support random field access. It should return non-empty [[Opt]] containing input for every field
    * present in the object, regardless of field order assumed by [[nextField]].
    * `Opt.Empty` is returned when field is absent or always when this `ObjectInput` does not support random field
    * access (in which case it must preserve field order instead).
    * NOTE: calling [[peekField]] and using [[com.avsystem.commons.serialization.FieldInput FieldInput]] returned by
    * it MUST NOT change state of this `ObjectInput`.
    * Therefore, it cannot in any way influence results returned by [[nextField]] and [[hasNext]].
    * For example, if a [[com.avsystem.commons.serialization.FieldInput FieldInput]] for particular field has already been
    * accessed using [[peekField]] but has not yet been returned by [[nextField]] then it MUST be returned at some
    * point in the future by [[nextField]].
    */
  def peekField(name: String): Opt[FieldInput] = Opt.Empty

  /**
    * Tries to obtain [[com.avsystem.commons.serialization.FieldInput FieldInput]] for field with specified name,
    * either by using [[peekField]] (assuming format with random field access) or [[nextField]] (assuming format that
    * preserves field order). A codec that uses this method must ensure that it reads fields in the same order as they
    * were written using `writeField` on [[ObjectOutput]].
    */
  def getNextNamedField(name: String): FieldInput =
    peekField(name).getOrElse {
      val fi = nextField()
      if (fi.fieldName != name) {
        throw new ReadFailure(s"Expected field $name, got ${fi.fieldName}")
      }
      fi
    }

  def skipRemaining(): Unit = while (hasNext) nextField().skip()
  def iterator[A](readFun: Input => A): Iterator[(String, A)] =
    new Iterator[(String, A)] {
      def hasNext: Boolean = self.hasNext
      def next(): (String, A) = {
        val fi = nextField()
        (fi.fieldName, readFun(fi))
      }
    }
}

/**
  * An `Input` representing an object field. The same as `Input` but also provides field name.
  */
trait FieldInput extends Input {
  def fieldName: String
}
