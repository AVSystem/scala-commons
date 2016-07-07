package com.avsystem.commons
package rpc.akka.serialization

import akka.util.{ByteIterator, ByteString}
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.rpc.akka.serialization.PrimitiveSizes._
import com.avsystem.commons.serialization.{Input, ListInput, ObjectInput, ReadFailed, ReadSuccessful, ValueRead}

/**
  * @author Wojciech Milewski
  */
private[akka] class ByteStringLinearInput(source: ByteString, onMove: Int => Unit) extends Input {

  def this(source: ByteString) = this(source, _ => Unit)

  import ByteOrderImplicits._

  override def readNull(): ValueRead[Null] = readCompileTime(NullMarker)(_ => null)
  override def readLong(): ValueRead[Long] = readCompileTime(LongMarker)(_.getLong)
  override def readInt(): ValueRead[Int] = readCompileTime(IntMarker)(_.getInt)
  override def readString(): ValueRead[String] = readRuntime(StringMarker)(_.utf8String)
  override def readBinary(): ValueRead[Array[Byte]] = readRuntime(ByteArrayMarker)(_.toArray)
  override def readList(): ValueRead[ListInput] = {
    source.headOption match {
      case Some(value) if value == ListStartMarker.byte =>
        onMove(ByteBytes)
        ReadSuccessful(new ByteStringLinearListInput(source.tail, onMove))
      case Some(value) => ReadFailed(s"Incorrect data has been found. Expected: ${ListStartMarker.byte.toInt}, but found: ${value.toInt}")
      case None => ReadFailed("No data found")
    }
  }
  override def readObject(): ValueRead[ObjectInput] = {
    source.headOption match {
      case Some(value) if value == ObjectStartMarker.byte =>
        onMove(ByteBytes)
        ReadSuccessful(new ByteStringLinearObjectInput(source.tail, onMove))
      case Some(value) => ReadFailed(s"Incorrect data has been found. Expected: ${ListStartMarker.byte.toInt}, but found: ${value.toInt}")
      case None => ReadFailed("No data found")
    }
  }

  override def readDouble(): ValueRead[Double] = readCompileTime(DoubleMarker)(_.getDouble)
  override def readBoolean(): ValueRead[Boolean] = readCompileTimeAsValue(BooleanMarker) { iterator =>
    iterator.head match {
      case 1 => ReadSuccessful(true)
      case 0 => ReadSuccessful(false)
      case value => ReadFailed(s"Found incorrect data: ${value.toInt}")
    }
  }
  override def skip(): Unit = {
    source.headOption.flatMap(Marker.of) match {
      case Some(value: CompileTimeSize) =>
        onMove(ByteBytes + value.size)
      case Some(ListStartMarker) => readList().foreach(_.skipRemaining())
      case Some(ObjectStartMarker) => readObject().foreach(_.skipRemaining())
      case Some(value: RuntimeSize) =>
        onMove(ByteBytes + IntBytes + source.iterator.drop(1).getInt)
      case Some(ListEndMarker) | Some(ObjectEndMarker) => onMove(ByteBytes)
      case None =>
    }
  }

  private def readCompileTime[T](marker: CompileTimeSize)(f: (ByteIterator) => T): ValueRead[T] = readCompileTimeAsValue(marker)(iterator => ReadSuccessful(f(iterator)))

  private def readCompileTimeAsValue[T](marker: CompileTimeSize)(f: (ByteIterator) => ValueRead[T]): ValueRead[T] = {
    if (source.size < ByteBytes + marker.size) ReadFailed(s"Source doesn't contain $marker and data")
    else if (source(0) != marker.byte) ReadFailed(s"Expected $marker, but another byte found: ${source(0).toInt}")
    else {
      onMove(ByteBytes + marker.size)
      f(source.iterator.drop(ByteBytes).take(marker.size))
    }
  }

  private def readRuntime[T](marker: RuntimeSize)(dataFun: ByteString => T): ValueRead[T] = {
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
}

private class ByteStringLinearListInput(private var source: ByteString, onMove: Int => Unit) extends ListInput {

  private var closed = false
  private var usedBytes = 0

  override def nextElement(): Input = new ByteStringLinearInput(source, bytes => {
    source = source.drop(bytes)
    usedBytes += bytes
  })

  override def hasNext: Boolean = {
    source.headOption match {
      case Some(value) if value == ListEndMarker.byte => closeIfNotClosed(); false
      case Some(value) => true
      case None => false
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
    source.headOption match {
      case Some(value) if value == ObjectEndMarker.byte => closeIfNotClosed(); false
      case Some(value) => true
      case None => false
    }

  }

  private def closeIfNotClosed(): Unit = {
    if (!closed) {
      onMove(ByteBytes + usedBytes)
      closed = true
    }
  }
}