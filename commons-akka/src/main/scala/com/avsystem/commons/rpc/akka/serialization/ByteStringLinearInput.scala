package com.avsystem.commons
package rpc.akka.serialization

import akka.util.{ByteIterator, ByteString}
import com.avsystem.commons.serialization.{Input, ListInput, ObjectInput, ReadFailed, ReadSuccessful, ValueRead}

/**
  * @author Wojciech Milewski
  */
private[akka] class ByteStringLinearInput(source: ByteString, onMove: Int => Unit) extends Input {

  def this(source: ByteString) = this(source, _ => ())

  import ByteOrderImplicits._

  override def readNull(): ValueRead[Null] = readStatic(NullMarker)(_ => null)
  override def readLong(): ValueRead[Long] = readStatic(LongMarker)(_.getLong)
  override def readInt(): ValueRead[Int] = readStatic(IntMarker)(_.getInt)
  override def readString(): ValueRead[String] = readDynamic(StringMarker)(_.utf8String)
  override def readBinary(): ValueRead[Array[Byte]] = readDynamic(ByteArrayMarker)(_.toArray)
  override def readList(): ValueRead[ListInput] = readBiMarker(ListStartMarker)(new ByteStringLinearListInput(_, onMove))
  override def readObject(): ValueRead[ObjectInput] = readBiMarker(ObjectStartMarker)(new ByteStringLinearObjectInput(_, onMove))
  override def readDouble(): ValueRead[Double] = readStatic(DoubleMarker)(_.getDouble)
  override def readBoolean(): ValueRead[Boolean] = readStaticAsValue(BooleanMarker) { iterator =>
    iterator.head match {
      case TrueByte => ReadSuccessful(true)
      case FalseByte => ReadSuccessful(false)
      case value => ReadFailed(s"Found incorrect data: ${value.toInt}")
    }
  }
  override def skip(): Unit = {
    if (source.nonEmpty) Marker.of(source.head).foreach {
      case value: StaticSize =>
        onMove(ByteBytes + value.size)
      case ListStartMarker => readList().foreach(_.skipRemaining())
      case ObjectStartMarker => readObject().foreach(_.skipRemaining())
      case value: DynamicSize =>
        onMove(ByteBytes + IntBytes + source.iterator.drop(1).getInt)
      case ListEndMarker | ObjectEndMarker => onMove(ByteBytes)
    }
  }

  private def readStatic[T](marker: StaticSize)(f: (ByteIterator) => T): ValueRead[T] = readStaticAsValue(marker)(iterator => ReadSuccessful(f(iterator)))

  private def readStaticAsValue[T](marker: StaticSize)(f: (ByteIterator) => ValueRead[T]): ValueRead[T] = {
    if (source.size < ByteBytes + marker.size) ReadFailed(s"Source doesn't contain $marker and data")
    else if (source(0) != marker.byte) ReadFailed(s"Expected $marker, but another byte found: ${source(0).toInt}")
    else {
      onMove(ByteBytes + marker.size)
      f(source.iterator.drop(ByteBytes).take(marker.size))
    }
  }

  private def readDynamic[T](marker: DynamicSize)(dataFun: ByteString => T): ValueRead[T] = {
    def contentSize = source.iterator.drop(ByteBytes).getInt
    val headerSize = ByteBytes + IntBytes

    if (source.size < headerSize) ReadFailed(s"Source doesn't contain $marker and length of serialized data")
    else if (source(0) != marker.byte) ReadFailed(s"Expected $marker, but another byte found: ${source(0).toInt}")
    else if (source.size < headerSize + contentSize) ReadFailed("Source doesn't contain declared byte array")
    else {
      onMove(headerSize + contentSize)
      ReadSuccessful(dataFun(source.slice(headerSize, headerSize + contentSize)))
    }
  }

  private def readBiMarker[T <: BiMarker, R](marker: T)(input: ByteString => R): ValueRead[R] = {
    if (source.isEmpty) ReadFailed("No data found")
    else source.head match {
      case value if value == marker.byte =>
        onMove(ByteBytes)
        ReadSuccessful(input(source.tail))
    }
  }
}

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

  override def nextField(): (String, Input) = {
    val key = new ByteStringLinearInput(source, bytes => {
      source = source.drop(bytes)
      usedBytes += bytes
    }).readString().get

    key -> new ByteStringLinearInput(source, bytes => {
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