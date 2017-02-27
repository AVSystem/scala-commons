package com.avsystem.commons
package rpc.akka.serialization

import akka.util.{ByteIterator, ByteString}
import com.avsystem.commons.rpc.akka.serialization.ByteOrderImplicits._
import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization.{FieldInput, Input, InputType, ListInput, ObjectInput}

/**
  * @author Wojciech Milewski
  */
private[akka] class ByteStringInput(source: ByteString) extends Input {

  def inputType = Marker.of(source.head).getOrElse(throw new ReadFailure(s"Unknown input type")) match {
    case NullMarker => InputType.Null
    case ListStartMarker => InputType.List
    case ObjectStartMarker => InputType.Object
    case _ => InputType.Simple
  }

  override def readNull(): Null = readStatic(NullMarker)(_ => null)
  override def readByte(): Byte = readStatic(ByteMarker)(_.head)
  override def readShort(): Short = readStatic(ShortMarker)(_.getShort)
  override def readInt(): Int = readStatic(IntMarker)(_.getInt)
  override def readLong(): Long = readStatic(LongMarker)(_.getLong)
  override def readFloat(): Float = readStatic(FloatMarker)(_.getFloat)
  override def readDouble(): Double = readStatic(DoubleMarker)(_.getDouble)
  override def readString(): String = readDynamic(StringMarker)(_.utf8String)
  override def readList(): ListInput = readDynamic(ListStartMarker)(sliced => new ByteArrayListInput(sliced))
  override def readObject(): ObjectInput = readDynamic(ObjectStartMarker)(sliced => new ByteArrayObjectInput(sliced))
  override def readBinary(): Array[Byte] = readDynamic(ByteArrayMarker)(_.toArray)

  override def readBoolean(): Boolean = readStaticAsValue(BooleanMarker) { iterator =>
    iterator.head match {
      case TrueByte => true
      case FalseByte => false
      case value => throw new ReadFailure(s"Found incorrect data: $value")
    }
  }
  override def skip(): Unit = ()

  private def readStatic[T](marker: StaticSize)(f: (ByteIterator) => T): T = readStaticAsValue(marker)(iterator => f(iterator))

  private def readStaticAsValue[T](marker: StaticSize)(f: (ByteIterator) => T): T = {
    if (source.size < ByteBytes + marker.size) throw new ReadFailure(s"Source doesn't contain $marker and data")
    else if (source(0) != marker.byte) throw new ReadFailure(s"Expected $marker, but another byte found")
    else f(source.iterator.drop(ByteBytes))
  }

  private def readDynamic[T](marker: DynamicSize)(data: ByteString => T): T = {
    def contentSize = source.iterator.drop(ByteBytes).getInt
    val headerSize = ByteBytes + IntBytes

    if (source.size < headerSize) throw new ReadFailure(s"Source doesn't contain $marker and length of serialized data")
    else if (source(0) != marker.byte) throw new ReadFailure(s"Expected $marker, but another byte found")
    else if (source.size < headerSize + contentSize) throw new ReadFailure("Source doesn't contain declared byte array")
    else data(source.slice(headerSize, headerSize + contentSize))
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

private final class ByteStringFieldInput(val fieldName: String, source: ByteString)
  extends ByteStringInput(source) with FieldInput

private final case class DataIndexes(startInclusiveIndex: Int, endExclusiveIndex: Int)

private final class ByteArrayObjectInput(private var content: ByteString) extends ObjectInput with SequentialInputOps {

  private var nextDataCache: Option[DataIndexes] = findNextData()

  private def findNextData(): Option[DataIndexes] = {
    content.headOption.flatMap(Marker.of(_).toOption) match {
      case Some(StringMarker) => content
        .lift(ByteBytes)
        .map(_.toInt)
        .map(nameSize => ByteBytes + IntBytes + nameSize)
        .flatMap(dataStartIndex => findDataIndex(content, dataStartIndex))
      case _ => None
    }

  }

  override def nextField(): FieldInput = {
    require(nextDataCache.isDefined)
    val name = content.slice(ByteBytes + IntBytes, nextDataCache.get.startInclusiveIndex).utf8String
    val resultInput = new ByteStringFieldInput(name, content.slice(nextDataCache.get.startInclusiveIndex, nextDataCache.get.endExclusiveIndex))
    content = content.drop(nextDataCache.get.endExclusiveIndex)
    nextDataCache = findNextData()
    resultInput
  }
  override def hasNext: Boolean = nextDataCache.isDefined
}

private trait SequentialInputOps {
  final def findDataIndex(content: ByteString, offset: Int): Option[DataIndexes] = {
    content.lift(offset).flatMap(Marker.of(_).toOption) match {
      case Some(m: StaticSize) => Some(DataIndexes(offset, offset + m.size + 1))
      case Some(m: DynamicSize) =>
        content.lift(offset + 1).map(_ => content.iterator.drop(offset + 1).getInt).map(size => DataIndexes(offset, offset + size + ByteBytes + IntBytes))
      case _ => None
    }
  }
}