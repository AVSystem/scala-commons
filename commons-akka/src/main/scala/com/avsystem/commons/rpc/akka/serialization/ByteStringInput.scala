package com.avsystem.commons
package rpc.akka.serialization

import akka.util.{ByteIterator, ByteString}
import com.avsystem.commons.rpc.akka.serialization.ByteOrderImplicits._
import com.avsystem.commons.rpc.akka.serialization.PrimitiveSizes._
import com.avsystem.commons.serialization.{Input, ListInput, ObjectInput, ReadFailed, ReadSuccessful, ValueRead}

/**
  * @author Wojciech Milewski
  */
private[akka] final class ByteStringInput(source: ByteString) extends Input {

  override def readNull(): ValueRead[Null] = readCompileTime(NullMarker)(_ => null)
  override def readByte(): ValueRead[Byte] = readCompileTime(ByteMarker)(_.head)
  override def readShort(): ValueRead[Short] = readCompileTime(ShortMarker)(_.getShort)
  override def readInt(): ValueRead[Int] = readCompileTime(IntMarker)(_.getInt)
  override def readLong(): ValueRead[Long] = readCompileTime(LongMarker)(_.getLong)
  override def readFloat(): ValueRead[Float] = readCompileTime(FloatMarker)(_.getFloat)
  override def readDouble(): ValueRead[Double] = readCompileTime(DoubleMarker)(_.getDouble)
  override def readString(): ValueRead[String] = readRuntime(StringMarker)(_.utf8String)
  override def readList(): ValueRead[ListInput] = readRuntime(ListStartMarker)(sliced => new ByteArrayListInput(sliced))
  override def readObject(): ValueRead[ObjectInput] = readRuntime(ObjectStartMarker)(sliced => new ByteArrayObjectInput(sliced))
  override def readBinary(): ValueRead[Array[Byte]] = readRuntime(ByteArrayMarker)(_.toArray)

  override def readBoolean(): ValueRead[Boolean] = readCompileTimeAsValue(BooleanMarker) { iterator =>
    iterator.head match {
      case 1 => ReadSuccessful(true)
      case 0 => ReadSuccessful(false)
      case value => ReadFailed(s"Found incorrect data: $value")
    }
  }
  override def skip(): Unit = ()

  private def readCompileTime[T](marker: CompileTimeSize)(f: (ByteIterator) => T): ValueRead[T] = readCompileTimeAsValue(marker)(iterator => ReadSuccessful(f(iterator)))

  private def readCompileTimeAsValue[T](marker: CompileTimeSize)(f: (ByteIterator) => ValueRead[T]): ValueRead[T] = {
    if (source.size < ByteBytes + marker.size) ReadFailed(s"Source doesn't contain $marker and data")
    else if (source(0) != marker.byte) ReadFailed(s"Expected $marker, but another byte found")
    else f(source.iterator.drop(ByteBytes))
  }

  private def readRuntime[T](marker: RuntimeSize)(data: ByteString => T): ValueRead[T] = {
    def contentSize = source.iterator.drop(ByteBytes).getInt
    val headerSize = ByteBytes + IntBytes

    if (source.size < headerSize) ReadFailed(s"Source doesn't contain $marker and length of serialized data")
    else if (source(0) != marker.byte) ReadFailed(s"Expected $marker, but another byte found")
    else if (source.size < headerSize + contentSize) ReadFailed("Source doesn't contain declared byte array")
    else ReadSuccessful(data(source.slice(headerSize, headerSize + contentSize)))
  }
}

private final class ByteArrayListInput(private var content: ByteString) extends ListInput with SequentialInputOps {
  private var nextEntryCache: Option[DataIndexes] = findDataIndex(content, 0)

  override def nextElement(): Input = {
    require(nextEntryCache.isDefined)
    val result = new ByteStringInput(content.slice(nextEntryCache.get.startInclusiveIndex, nextEntryCache.get.endExclusiveIndex))
    content = content.drop(nextEntryCache.get.endExclusiveIndex)
    nextEntryCache = findDataIndex(content, 0)
    result
  }
  override def hasNext: Boolean = nextEntryCache.isDefined
}

private final case class DataIndexes(startInclusiveIndex: Int, endExclusiveIndex: Int)

private final class ByteArrayObjectInput(private var content: ByteString) extends ObjectInput with SequentialInputOps {

  private var nextDataCache: Option[DataIndexes] = findNextData()

  private def findNextData(): Option[DataIndexes] = {
    content.headOption.flatMap(Marker.of) match {
      case Some(StringMarker) => content
        .lift(ByteBytes)
        .map(_.toInt)
        .map(nameSize => ByteBytes + IntBytes + nameSize)
        .flatMap(dataStartIndex => findDataIndex(content, dataStartIndex))
      case _ => None
    }
  }

  override def nextField(): (String, Input) = {
    require(nextDataCache.isDefined)
    val name = content.slice(ByteBytes + IntBytes, nextDataCache.get.startInclusiveIndex).utf8String
    val resultInput = new ByteStringInput(content.slice(nextDataCache.get.startInclusiveIndex, nextDataCache.get.endExclusiveIndex))
    content = content.drop(nextDataCache.get.endExclusiveIndex)
    nextDataCache = findNextData()
    name -> resultInput
  }
  override def hasNext: Boolean = nextDataCache.isDefined
}

private trait SequentialInputOps {
  final def findDataIndex(content: ByteString, offset: Int): Option[DataIndexes] = {
    content.lift(offset).flatMap(Marker.of) match {
      case Some(m: CompileTimeSize) => Some(DataIndexes(offset, offset + m.size + 1))
      case Some(m: RuntimeSize) =>
        content.lift(offset + 1).map(_ => content.iterator.drop(offset + 1).getInt).map(size => DataIndexes(offset, offset + size + ByteBytes + IntBytes))
      case _ => None
    }
  }
}