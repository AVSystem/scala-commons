package com.avsystem.commons
package rpc.akka.serialization

import akka.util.{ByteIterator, ByteString}
import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization.{FieldInput, Input, InputType, ListInput, ObjectInput}

/**
  * @author Wojciech Milewski
  */
private[akka] class ByteStringLinearInput(source: ByteString, onMove: Int => Unit) extends Input {

  def this(source: ByteString) = this(source, _ => ())

  import ByteOrderImplicits._

  def inputType = Marker.of(source.head).getOrElse(throw new ReadFailure(s"Unknown input type")) match {
    case NullMarker => InputType.Null
    case ListStartMarker => InputType.List
    case ObjectStartMarker => InputType.Object
    case _ => InputType.Simple
  }

  override def readNull(): Null = readStatic(NullMarker)(_ => null)
  override def readLong(): Long = readStatic(LongMarker)(_.getLong)
  override def readInt(): Int = readStatic(IntMarker)(_.getInt)
  override def readString(): String = readDynamic(StringMarker)(_.utf8String)
  override def readBinary(): Array[Byte] = readDynamic(ByteArrayMarker)(_.toArray)
  override def readList(): ListInput = readBiMarker(ListStartMarker)(new ByteStringLinearListInput(_, onMove))
  override def readObject(): ObjectInput = readBiMarker(ObjectStartMarker)(new ByteStringLinearObjectInput(_, onMove))
  override def readDouble(): Double = readStatic(DoubleMarker)(_.getDouble)
  override def readBoolean(): Boolean = readStaticAsValue(BooleanMarker) { iterator =>
    iterator.head match {
      case TrueByte => true
      case FalseByte => false
      case value => throw new ReadFailure(s"Found incorrect data: ${value.toInt}")
    }
  }
  override def skip(): Unit = {
    if (source.nonEmpty) Marker.of(source.head).foreach {
      case value: StaticSize =>
        onMove(ByteBytes + value.size)
      case ListStartMarker => readList().skipRemaining()
      case ObjectStartMarker => readObject().skipRemaining()
      case value: DynamicSize =>
        onMove(ByteBytes + IntBytes + source.iterator.drop(1).getInt)
      case ListEndMarker | ObjectEndMarker => onMove(ByteBytes)
    }
  }

  private def readStatic[T](marker: StaticSize)(f: (ByteIterator) => T): T = readStaticAsValue(marker)(iterator => f(iterator))

  private def readStaticAsValue[T](marker: StaticSize)(f: (ByteIterator) => T): T = {
    if (source.size < ByteBytes + marker.size) throw new ReadFailure(s"Source doesn't contain $marker and data")
    else if (source(0) != marker.byte) throw new ReadFailure(s"Expected $marker, but another byte found: ${source(0).toInt}")
    else {
      onMove(ByteBytes + marker.size)
      f(source.iterator.drop(ByteBytes).take(marker.size))
    }
  }

  private def readDynamic[T](marker: DynamicSize)(dataFun: ByteString => T): T = {
    def contentSize = source.iterator.drop(ByteBytes).getInt
    val headerSize = ByteBytes + IntBytes

    if (source.size < headerSize) throw new ReadFailure(s"Source doesn't contain $marker and length of serialized data")
    else if (source(0) != marker.byte) throw new ReadFailure(s"Expected $marker, but another byte found: ${source(0).toInt}")
    else if (source.size < headerSize + contentSize) throw new ReadFailure("Source doesn't contain declared byte array")
    else {
      onMove(headerSize + contentSize)
      dataFun(source.slice(headerSize, headerSize + contentSize))
    }
  }

  private def readBiMarker[T <: BiMarker, R](marker: T)(input: ByteString => R): R = {
    if (source.isEmpty) throw new ReadFailure("No data found")
    else source.head match {
      case value if value == marker.byte =>
        onMove(ByteBytes)
        input(source.tail)
    }
  }
}

private class ByteStringLinearFieldInput(val fieldName: String, source: ByteString, onMove: Int => Unit)
  extends ByteStringLinearInput(source, onMove) with FieldInput

private class ByteStringLinearListInput(private var source: ByteString, onMove: Int => Unit) extends ListInput {

  private var closed = false
  private var usedBytes = 0

  override def nextElement(): Input = new ByteStringLinearInput(source, bytes => {
    source = source.drop(bytes)
    usedBytes += bytes
  })

  override def hasNext: Boolean = {
    if (source.isEmpty) false
    else source.head match {
      case value if value == ListEndMarker.byte => closeIfNotClosed(); false
      case _ => true
    }
  }

  private def closeIfNotClosed(): Unit = {
    if (!closed) {
      onMove(ByteBytes + usedBytes)
      closed = true
    }
  }
}

private class ByteStringLinearObjectInput(private var source: ByteString, onMove: Int => Unit) extends ObjectInput {

  private var closed = false
  private var usedBytes = 0

  override def nextField(): FieldInput = {
    val key = new ByteStringLinearInput(source, bytes => {
      source = source.drop(bytes)
      usedBytes += bytes
    }).readString()

    new ByteStringLinearFieldInput(key, source, bytes => {
      source = source.drop(bytes)
      usedBytes += bytes
    })
  }

  override def hasNext: Boolean = {
    if (source.isEmpty) false
    else source.head match {
      case value if value == ObjectEndMarker.byte => closeIfNotClosed(); false
      case _ => true
    }

  }

  private def closeIfNotClosed(): Unit = {
    if (!closed) {
      onMove(ByteBytes + usedBytes)
      closed = true
    }
  }
}