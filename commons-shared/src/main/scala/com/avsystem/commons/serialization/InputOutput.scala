package com.avsystem.commons
package serialization

import com.avsystem.commons.serialization.GenCodec.ReadFailure

/**
  * Represents an abstract sink to which a value may be serialized (written).
  * An [[Output]] instance should be assumed to be stateful. After calling any of the `write` methods, it MUST NOT be
  * reused. This means that [[Output]] instance can be used only to write a single value. However, if the value
  * to write is complex, one can use `writeList`/`writeSet` or `writeObject`/`writeMap`.
  */
trait Output extends Any {
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
  def writeBinary(binary: Array[Byte]): Unit
  def writeList(): ListOutput
  def writeSet(): ListOutput = writeList()
  def writeObject(): ObjectOutput
  def writeMap(): ObjectOutput = writeObject()
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
trait ListOutput extends SequentialOutput {
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
trait ObjectOutput extends SequentialOutput {
  /**
    * Returns an [[com.avsystem.commons.serialization.Output Output]] representing value mapped to given string key.
    * This [[com.avsystem.commons.serialization.Output Output]] instance must be fully
    * used before calling [[writeField]] next time. That means, one can NOT simultaneously use multiple instances
    * of [[com.avsystem.commons.serialization.Output Output]] returned by subsequent calls to this method.
    */
  def writeField(key: String): Output
}

/**
  * Represents the type of value inside and [[Input]] that can be read from it.
  * <p/>
  * It is possible to distinguish only between four types (null, simple value, object and list) even though
  * any of these types may have different representations. For example, [[InputType.Simple]] is returned for
  * more than one actual value types (numbers, booleans, strings, timestamps, binary, etc.).
  * <p/>
  * It's not possible to distinguish between them based only on [[InputType]] because not every [[Input]]
  * implementation is able to do that. For example, JSON must represent 64-bit integers as strings and therefore
  * it can't distinguish between strings and numbers in general.
  */
sealed trait InputType
object InputType {
  case object Null extends InputType
  case object Simple extends InputType
  case object Object extends InputType
  case object List extends InputType
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
  /**
    * Returns the type of the value that can be read from this [[com.avsystem.commons.serialization.Input Input]].
    * Only four types can be distinguished (see [[com.avsystem.commons.serialization.InputType InputType]] for more details on this).
    * <p/>
    * If this method returns [[com.avsystem.commons.serialization.InputType.Null InputType.Null]],
    * then `readNull()` can be safely called.<br/>
    * If this method returns [[com.avsystem.commons.serialization.InputType.Object InputType.Object]],
    * then AT LEAST ONE OF `readObject()` and `readMap()` can be safely called.<br/>
    * If this method returns [[com.avsystem.commons.serialization.InputType.List InputType.List]],
    * then AT LEAST ONE OF `readList()` and `readSet()` can be safely called.<br/>
    * If this method returns [[com.avsystem.commons.serialization.InputType.Simple InputType.Simple]]
    * then AT LEAST ONE OF `readString()`, `readChar()`, `readBoolean()`,
    * `readByte()`, `readShort()`, `readInt()`, `readLong()`, `readTimestamp()`, `readFloat()`, `readDouble()`,
    * `readBinary()` can be called.
    * <p/>
    * It's impossible to know which of the listed methods is actually safe to call based only on
    * [[com.avsystem.commons.serialization.InputType InputType]].
    * It is the responsibility of [[com.avsystem.commons.serialization.GenCodec GenCodec]] implementation to have
    * reading and writing logic consistent.
    * For example, if `writeDouble(Double)` is used during writing then `readDouble()` must be used during reading
    * by the same [[com.avsystem.commons.serialization.GenCodec GenCodec]].
    *
    * @return
    */
  def inputType: InputType
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
  def readBinary(): Array[Byte]
  def readList(): ListInput
  def readSet(): ListInput = readList()
  def readObject(): ObjectInput
  def readMap(): ObjectInput = readObject()
  def skip(): Unit
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
trait ListInput extends SequentialInput { self =>
  /**
    * Returns an [[com.avsystem.commons.serialization.Input Input]] representing next element in a sequence of values
    * represented by this [[com.avsystem.commons.serialization.ListInput ListInput]].
    * Returned [[com.avsystem.commons.serialization.Input Input]] instance must be fully exhausted before calling
    * `nextElement()` next time.
    */
  def nextElement(): Input

  def skipRemaining() = while (hasNext) nextElement().skip()
  def iterator[A](readFun: Input => A): Iterator[A] =
    new Iterator[A] {
      def hasNext = self.hasNext
      def next() = readFun(nextElement())
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
trait ObjectInput extends SequentialInput { self =>
  /**
    * Returns [[com.avsystem.commons.serialization.FieldInput FieldInput]] that represents next field of this object.
    * You MUST NOT call `nextField()` again until this [[com.avsystem.commons.serialization.FieldInput FieldInput]]
    * is fully read or skipped.
    * </p>
    * Subsequent invocations of `nextField` MUST return fields in exactly the same order as they were written
    * using [[com.avsystem.commons.serialization.ObjectOutput.writeField ObjectOutput.writeField]].
    * In other words, serialization format MUST preserve order of object fields.
    */
  def nextField(): FieldInput

  def skipRemaining() = while (hasNext) nextField().skip()
  def iterator[A](readFun: Input => A): Iterator[(String, A)] =
    new Iterator[(String, A)] {
      def hasNext = self.hasNext
      def next() = {
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

  def assertField(expectedName: String): this.type = {
    if (fieldName != expectedName) {
      throw new ReadFailure(s"Expected $expectedName as next field, got $fieldName")
    }
    this
  }
}
