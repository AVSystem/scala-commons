package com.avsystem.commons
package serialization

import com.avsystem.commons.jiop.BasicJavaInterop._
import com.avsystem.commons.misc.Unboxing

import scala.collection.mutable.ListBuffer
import scala.reflect.{ClassTag, classTag}

class SimpleValueInput(value: Any) extends Input {
  private def doRead[A: ClassTag]: ValueRead[A] =
    doReadUnboxed[A, A](classTag[A], Unboxing(identity))

  private def doReadUnboxed[A, B: ClassTag](implicit unboxing: Unboxing[A, B]): ValueRead[A] = value match {
    case b: B => ReadSuccessful(unboxing.fun(b))
    case _ => ReadFailed(s"Expected ${classTag[B].runtimeClass} but got ${value.getClass}")
  }

  def readBinary() = doRead[Array[Byte]]
  def readLong() = doReadUnboxed[Long, JLong]
  def readNull() = if (value == null) ReadSuccessful(null) else ReadFailed("not null")
  def readObject() = doRead[Map[String, Any]].map { theMap =>
    new ObjectInput {
      private val it = theMap.iterator.map {
        case (k, v) => (k, new SimpleValueInput(v))
      }
      def nextField() = it.next()
      def hasNext = it.hasNext
    }
  }

  def readInt() = doReadUnboxed[Int, JInteger]
  def readString() = doRead[String]

  def readList() = doRead[List[Any]].map { theList =>
    new ListInput {
      private val it = theList.iterator.map(new SimpleValueInput(_))
      def nextElement() = it.next()
      def hasNext = it.hasNext
    }
  }

  def readBoolean() = doReadUnboxed[Boolean, JBoolean]
  def readDouble() = doReadUnboxed[Double, JDouble]

  def skip() = ()
}

class SimpleValueOutput(consumer: Any => Unit) extends Output {
  def writeBinary(binary: Array[Byte]) = consumer(binary)
  def writeString(str: String) = consumer(str)
  def writeDouble(double: Double) = consumer(double)
  def writeInt(int: Int) = consumer(int)

  def writeList() = new ListOutput {
    private val buffer = new ListBuffer[Any]
    def writeElement() = new SimpleValueOutput(buffer += _)
    def finish() = consumer(buffer.result())
  }

  def writeBoolean(boolean: Boolean) = consumer(boolean)

  def writeObject() = new ObjectOutput {
    private val builder = Map.newBuilder[String, Any]
    def writeField(key: String) = new SimpleValueOutput(v => builder += ((key, v)))
    def finish() = consumer(builder.result())
  }

  def writeLong(long: Long) = consumer(long)
  def writeNull() = consumer(null)
}
