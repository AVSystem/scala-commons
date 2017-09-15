package com.avsystem.commons
package serialization

import com.avsystem.commons.serialization.GenCodec.NullSafeCodec
import com.avsystem.commons.serialization.PolymorphicGenCodec.Variant

class PolymorphicGenCodec[T](variants: Variant[_ <: T]*) extends NullSafeCodec[T] {
  private val nameToVariant = variants.iterator.mkMap(_.name, identity)

  private lazy val supportedTypesDebug = variants.map { v =>
    s"${v.name} -> ${v.ct.runtimeClass.getName}"
  }.mkString(", ")

  override protected def nullable: Boolean = true

  override protected def readNonNull(input: Input): T = {
    val objectInput = input.readObject()
    val tpe = objectInput.nextField().assertField(PolymorphicGenCodec.ObjectTypeField).readString()
    val variant = nameToVariant.getOrElse(
      tpe,
      throw new IllegalArgumentException(s"Unsupported type marker: $tpe. Supported types are: $supportedTypesDebug")
    )
    variant.readFrom(objectInput)
  }

  override protected def writeNonNull(output: Output, value: T): Unit = {
    val objectOutput = output.writeObject()
    variants.find(_.maybeWriteTo(objectOutput, value)).getOrElse {
      throw new IllegalArgumentException(s"Unsupported object: $value of type ${value.getClass.getName}. Supported types are: $supportedTypesDebug")
    }
  }
}

object PolymorphicGenCodec {
  private class CustomObjectInput(objectInput: ObjectInput) extends Input {
    override def inputType: InputType = InputType.Object
    override def readNull(): Null = throw new UnsupportedOperationException
    override def readString(): String = throw new UnsupportedOperationException
    override def readBoolean(): Boolean = throw new UnsupportedOperationException
    override def readInt(): Int = throw new UnsupportedOperationException
    override def readLong(): Long = throw new UnsupportedOperationException
    override def readDouble(): Double = throw new UnsupportedOperationException
    override def readBinary(): Array[Byte] = throw new UnsupportedOperationException
    override def readList(): ListInput = throw new UnsupportedOperationException
    override def skip(): Unit = throw new UnsupportedOperationException

    override def readObject(): ObjectInput = objectInput
  }

  private class CustomObjectOutput(objectOutput: ObjectOutput) extends Output {
    override def writeNull(): Unit = throw new UnsupportedOperationException
    override def writeString(str: String): Unit = throw new UnsupportedOperationException
    override def writeBoolean(boolean: Boolean): Unit = throw new UnsupportedOperationException
    override def writeInt(int: Int): Unit = throw new UnsupportedOperationException
    override def writeLong(long: Long): Unit = throw new UnsupportedOperationException
    override def writeDouble(double: Double): Unit = throw new UnsupportedOperationException
    override def writeBinary(binary: Array[Byte]): Unit = throw new UnsupportedOperationException
    override def writeList(): ListOutput = throw new UnsupportedOperationException

    override def writeObject(): ObjectOutput = objectOutput
  }

  val ObjectTypeField = "objType"

  case class Variant[T](name: String)(implicit val ct: ClassTag[T], val codec: GenCodec[T]) {
    def readFrom(objectInput: ObjectInput): T = {
      codec.read(new CustomObjectInput(objectInput))
    }

    def maybeWriteTo[U >: T](objectOutput: ObjectOutput, value: U): Boolean = {
      value match {
        case ct(t) =>
          objectOutput.writeField(ObjectTypeField).writeString(name)
          codec.write(new CustomObjectOutput(objectOutput), t)
          true
        case _ =>
          false
      }
    }
  }
}
