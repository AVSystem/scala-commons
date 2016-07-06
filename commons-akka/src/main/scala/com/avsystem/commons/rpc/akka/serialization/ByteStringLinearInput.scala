package com.avsystem.commons
package rpc.akka.serialization

import akka.util.{ByteIterator, ByteString}
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.rpc.akka.serialization.PrimitiveSizes._
import com.avsystem.commons.serialization.{Input, ListInput, ObjectInput, ReadFailed, ReadSuccessful, ValueRead}

/**
  * @author Wojciech Milewski
  */
private[akka] class ByteStringLinearInput(source: ByteString, listener: Option[Listener]) extends Input with Listener {

  def this(source: ByteString) = this(source, None)

  import ByteOrderImplicits._

  override def readNull(): ValueRead[Null] = readCompileTime(NullMarker)(_ => null)
  override def readLong(): ValueRead[Long] = readCompileTime(LongMarker)(_.getLong)
  override def readInt(): ValueRead[Int] = readCompileTime(IntMarker)(_.getInt)
  override def readString(): ValueRead[String] = readRuntime(StringMarker)(_.utf8String)
  override def readBinary(): ValueRead[Array[Byte]] = readRuntime(ByteArrayMarker)(_.toArray)
  override def readList(): ValueRead[ListInput] = {
    source.headOption match {
      case Some(value) if value == ListStartMarker.byte =>
        listener.foreach(_.dataUsed(ByteBytes))
        ReadSuccessful(new ByteStringLinearListInput(source.tail, listener))
      case Some(value) => ReadFailed(s"Incorrect data has been found. Expected: ${ListStartMarker.byte.toInt}, but found: ${value.toInt}")
      case None => ReadFailed("No data found")
    }
  }
  override def readObject(): ValueRead[ObjectInput] = {
    source.headOption match {
      case Some(value) if value == ObjectStartMarker.byte =>
        listener.foreach(_.dataUsed(ByteBytes))
        ReadSuccessful(new ByteStringLinearObjectInput(source.tail, listener))
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
      case Some(value: CompileTimeSize) => listener.foreach(_.dataUsed(ByteBytes + value.size))
      case Some(ListStartMarker) => readList().foreach(_.skipRemaining())
      case Some(ObjectStartMarker) => readObject().foreach(_.skipRemaining())
      case Some(value: RuntimeSize) => listener.foreach(_.dataUsed(ByteBytes + IntBytes + source.iterator.drop(1).getInt))
      case Some(ListEndMarker) | Some(ObjectEndMarker) => listener.foreach(_.dataUsed(ByteBytes))
      case None =>
    }
  }

  private def readCompileTime[T](marker: CompileTimeSize)(f: (ByteIterator) => T): ValueRead[T] = readCompileTimeAsValue(marker)(iterator => ReadSuccessful(f(iterator)))

  private def readCompileTimeAsValue[T](marker: CompileTimeSize)(f: (ByteIterator) => ValueRead[T]): ValueRead[T] = {
    if (source.size < ByteBytes + marker.size) ReadFailed(s"Source doesn't contain $marker and data")
    else if (source(0) != marker.byte) ReadFailed(s"Expected $marker, but another byte found: ${source(0).toInt}")
    else {
      listener.foreach(_.dataUsed(ByteBytes + marker.size))
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
      listener.foreach(_.dataUsed(headerSize + contentSize))
      ReadSuccessful(dataFun(source.slice(headerSize, headerSize + contentSize)))
    }
  }
  override def dataUsed(bytes: Int): Unit = listener.foreach(_.dataUsed(bytes))
}

private class ByteStringLinearListInput(private val container: Container, listener: Option[Listener]) extends ListInput with Listener {self =>

  private var closed = false

  def this(bs: ByteString, listener: Option[Listener]) = this(new Container(bs), listener)

  override def nextElement(): Input = new ByteStringLinearInput(container.source, Some(this))

  override def hasNext: Boolean = {
    container.source.headOption match {
      case Some(value) if value == ListEndMarker.byte => closeIfNotClosed(); false
      case Some(value) => true
      case None => false
    }
  }
  override def dataUsed(bytes: Int): Unit = {
    container.move(bytes)
    listener.foreach(_.dataUsed(bytes))
  }

  private def closeIfNotClosed(): Unit = {
    if (!closed) {
      listener.foreach(_.dataUsed(ByteBytes))
      closed = true
    }
  }
}

private class ByteStringLinearObjectInput(private var source: ByteString, listener: Option[Listener]) extends ObjectInput with Listener {

  private var closed = false

  override def nextField(): (String, Input) = {
    val key = new ByteStringLinearInput(source, Some(this)).readString().get

    key -> new ByteStringLinearInput(source, Some(this))
  }

  override def hasNext: Boolean = {
    source.headOption match {
      case Some(value) if value == ObjectEndMarker.byte => closeIfNotClosed(); false
      case Some(value) => true
      case None => false
    }

  }
  override def dataUsed(bytes: Int): Unit = {
    source = source.drop(bytes)
    listener.foreach(_.dataUsed(bytes))
  }

  private def closeIfNotClosed(): Unit = {
    if (!closed) {
      listener.foreach(_.dataUsed(ByteBytes))
      closed = true
    }
  }
}

private object ByteStringLinearInput {
  implicit class ByteIteratorOps(private val iterator: ByteIterator) extends AnyVal {
    def nextOpt: Opt[Byte] = if (iterator.hasNext) Opt(iterator.next()) else Opt.empty
  }
}

trait Listener {
  def dataUsed(bytes: Int): Unit
}

class Container(var source: ByteString) {
  def move(bytes: Int): Unit = {
    source = source.drop(bytes)
  }
}