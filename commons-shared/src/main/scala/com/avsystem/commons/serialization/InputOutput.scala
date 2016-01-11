package com.avsystem.commons
package serialization

/**
  * Represents an abstract sink to which a value may be serialized (written).
  * An [[Output]] instance should be assumed to be stateful. After calling any of the `write` methods, it MUST NOT be
  * reused. This means that [[Output]] instance can be used only to write a single value. However, if the value
  * to write is complex, one can use `writeList`/`writeSet` or `writeObject`/`writeMap`.
  */
trait Output {
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
trait SequentialOutput {
  /**
    * Indicates that all elements or fields in this [[SequentialOutput]] have been written. This method MUST always
    * be called after list/object writing has been finished.
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
    * Returns an [[Output]] representing next element in this list. This [[Output]] instance MUST be fully used
    * before calling [[writeElement]] next time. That means, one can NOT simultaneously use multiple instances of
    * [[Output]] returned by subsequent calls to this method.
    */
  def writeElement(): Output
}
/**
  * Represents an abstract sink for serialization of string-to-value mappings. Any [[ObjectOutput]] instance
  * must be assumed to be stateful and used in strictly sequential manner. After all key-value pairs have been
  * written, `finish()` must be called to explicitly mark that the object is complete.
  */
trait ObjectOutput extends SequentialOutput {
  /**
    * Returns an [[Output]] representing value mapped to given string key. This [[Output]] instance must be fully
    * used before calling [[writeField]] next time. That means, one can NOT simultaneously use multiple instances
    * of [[Output]] returned by subsequent calls to this method.
    */
  def writeField(key: String): Output
}

/**
  * Represents an abstract source from which a value may be deserialized (read).
  * Each of the `read` methods returns an instance of [[ValueRead]] which may contain either a successfully read
  * value or an error message.
  * <p/>
  * An [[Input]] value should be assumed to be stateful. If any of the `read` methods have already been called AND
  * returned successful result (([[ReadSuccessful]]), the [[Input]] instance can no longer be used and MUST be discarded.
  * On the other hand, when the `read` method returned a [[ReadFailed]], its state should be unchanged and any `read`
  * method may be called again.
  * <p/>
  * In order to ignore the value kept in this [[Input]], `skip()` MUST be called.
  * <p/>
  * In summary: every [[Input]] MUST be fully exhausted by either calling one of the `read` methods which returns
  * successful value or by calling `skip()`. Also, [[ListInput]] and [[ObjectInput]] instances returned from this
  * [[Input]] must also be fully exhausted on their own.
  */
trait Input {
  def readNull(): ValueRead[Null]
  def readUnit(): ValueRead[Unit] = readNull().map(_ => ())
  def readString(): ValueRead[String]
  def readChar(): ValueRead[Char] = readString().map(_.charAt(0))
  def readBoolean(): ValueRead[Boolean]
  def readByte(): ValueRead[Byte] = readShort().map(_.toByte)
  def readShort(): ValueRead[Short] = readInt().map(_.toShort)
  def readInt(): ValueRead[Int]
  def readLong(): ValueRead[Long]
  def readTimestamp(): ValueRead[Long] = readLong()
  def readFloat(): ValueRead[Float] = readDouble().map(_.toFloat)
  def readDouble(): ValueRead[Double]
  def readBinary(): ValueRead[Array[Byte]]
  def readList(): ValueRead[ListInput]
  def readSet(): ValueRead[ListInput] = readList()
  def readObject(): ValueRead[ObjectInput]
  def readMap(): ValueRead[ObjectInput] = readObject()
  def skip(): Unit
}
trait SequentialInput {
  def hasNext: Boolean
  def skipRemaining(): Unit
}
/**
  * Represents an abstract source of sequence of values that can be deserialized.
  * [[ListInput]] instance is stateful and MUST be read strictly sequentially. This means, you MUST fully exhaust
  * an [[Input]] instance returned by `nextElement()` before calling `nextElement()` again. For this reason,
  * [[ListInput]] is not an [[Iterator]] despite having similar interface ([[Iterator]] would easily allow e.g. conversion
  * to `List[Input]` which would be illegal).
  * <p/>
  * [[ListInput]] MUST always be fully exhausted. In order to ignore any remaining elements, skipRemaining() may be
  * used.
  */
trait ListInput extends SequentialInput {self =>
  /**
    * Returns an [[Input]] representing next element in a sequence of values represented by this [[ListInput]].
    * Returned [[Input]] instance must be fully exhausted before calling `nextElement()` next time.
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
  * [[ObjectInput]] instance is stateful and MUST be read strictly sequentially. This means, you MUST fully exhaust
  * any [[Input]] instance returned by `nextField()` before calling `nextField()` again. For this reason,
  * [[ObjectInput]] is not an [[Iterator]] despite having similar interface ([[Iterator]] would easily allow e.g.
  * conversion to `List[(String, Input)]` which would be illegal).
  * <p/>
  * [[ObjectInput]] MUST always be fully exhausted. In order to ignore any remaining key-value mappings,
  * `skipRemaining()` may be used.
  * <p/>
  * NOTE: The order of keys returned by subsequent invocations of `nextField()` may be arbitrary and in particular
  * may not match the order in which keys were written to corresponding [[ObjectOutput]].
  */
trait ObjectInput extends SequentialInput {self =>
  def nextField(): (String, Input)

  def skipRemaining() = while (hasNext) nextField()._2.skip()
  def iterator[A](readFun: Input => A): Iterator[(String, A)] =
    new Iterator[(String, A)] {
      def hasNext = self.hasNext
      def next() = {
        val (k, i) = nextField()
        (k, readFun(i))
      }
    }
}
