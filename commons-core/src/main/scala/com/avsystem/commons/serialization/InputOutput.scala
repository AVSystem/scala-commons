package com.avsystem.commons
package serialization

import com.avsystem.commons.serialization.GenCodec.ReadFailure

/**
  * Base trait for type markers identifying custom native types that particular [[Input]] and [[Output]]
  * implementations might want to support.
  */
trait TypeMarker[T]

/**
  * Represents an abstract sink to which a value may be serialized (written).
  * An [[Output]] instance should be assumed to be stateful. After calling any of the `write` methods, it MUST NOT be
  * reused. This means that [[Output]] instance can be used only to write a single value. However, if the value
  * to write is complex, one can use `writeList`/`writeSet` or `writeObject`/`writeMap`.
  */
trait Output extends Any {
  def writeSimple(): SimpleOutput
  def writeList(): ListOutput
  def writeObject(): ObjectOutput

  /**
    * This ugly workaround has been introduced when standard `Option` encoding changed from zero-or-one element list
    * encoding to unwrapped-or-null encoding which effectively disallowed serializing `null` and `Some(null)`.
    * If some `Output` implementation still wants to use the list encoding, it may do it by overriding this method
    * and returning `true`.
    */
  def legacyOptionEncoding: Boolean = false
}

trait SimpleOutput extends Any {
  def writeNull(): Unit
  def writeUnit(): Unit = writeNull()
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
  def writeBigInt(bigInt: BigInt): Unit
  def writeBigDecimal(bigDecimal: BigDecimal): Unit
  def writeBinary(binary: Array[Byte]): Unit
  def writeCustom[T](typeMarker: TypeMarker[T], value: T): Boolean = false
}

trait OutputAndSimpleOutput extends Any with Output with SimpleOutput {
  override final def writeSimple(): SimpleOutput = this
}

/**
  * Base trait for outputs which allow writing of multiple values in sequence, i.e. [[ListOutput]] and [[ObjectOutput]].
  */
trait SequentialOutput extends Any {
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
  * An [[Input]] value should be assumed to be stateful. If any of the `readX` methods have already been called,
  * the [[Input]] instance can no longer be used and MUST be discarded.
  * <p/>
  * In order to ignore the value kept in this [[Input]], `skip()` MUST be called.
  * <p/>
  * In summary: every [[Input]] MUST be fully exhausted by either calling one of the `read` methods which returns
  * successful value or by calling `skip()`. Also, [[ListInput]] and [[ObjectInput]] instances returned from this
  * [[Input]] must also be fully exhausted on their own.
  */
trait Input extends Any {
  def isNull: Boolean
  def readSimple(): SimpleInput
  def readList(): ListInput
  def readObject(): ObjectInput
  def skip(): Unit

  /**
    * This ugly workaround has been introduced when standard `Option` encoding changed from zero-or-one element list
    * encoding to unwrapped-or-null encoding which effectively disallowed serializing `null` and `Some(null)`.
    * If some `Input` implementation still wants to use the list encoding, it may do it by overriding this method
    * and returning `true`.
    */
  def legacyOptionEncoding: Boolean = false
}

trait SimpleInput extends Any {
  def readNull(): Null
  def readUnit(): Unit = readNull()
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
  def readCustom[T](typeMarker: TypeMarker[T]): Opt[T] = Opt.Empty
}

trait InputAndSimpleInput extends Any with Input with SimpleInput {
  override final def readSimple(): SimpleInput = this
}

trait SequentialInput extends Any {
  def hasNext: Boolean
  def skipRemaining(): Unit
}
/**
  * Represents an abstract source of sequence of values that can be deserialized.
  * [[ListInput]] instance is stateful and MUST be read strictly sequentially. This means, you MUST fully exhaust
  * an [[Input]] instance returned by `nextElement()` before calling `nextElement()` again. For this reason,
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
    * NOTE: calling [[peekField]] and using [[FieldInput]] returned by it MUST NOT change state of this `ObjectInput`.
    * Therefore, it cannot in any way influence results returned by [[nextField]] and [[hasNext]].
    * For example, if a [[FieldInput]] for particular field has already been
    * accessed using [[peekField]] but has not yet been returned by [[nextField]] then it MUST be returned at some
    * point in the future by [[nextField]].
    */
  def peekField(name: String): Opt[FieldInput] = Opt.Empty

  /**
    * Tries to obtain [[FieldInput]] for field with specified name, either by using [[peekField]] (assuming format with
    * random field access) or [[nextField]] (assuming format that preserves field order). A codec that uses this method
    * must ensure that it reads fields in the same order as they were written using `writeField` on [[ObjectOutput]].
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
  * An [[Input]] representing an object field. The same as [[Input]] but also provides field name.
  */
trait FieldInput extends Input {
  def fieldName: String
}
