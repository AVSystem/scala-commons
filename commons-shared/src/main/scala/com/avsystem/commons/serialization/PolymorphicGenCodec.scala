package com.avsystem.commons
package serialization

import com.avsystem.commons.serialization.GenCodec.NullSafeCodec
import com.avsystem.commons.serialization.PolymorphicGenCodec.Variant

class PolymorphicGenCodec[T](variants: Variant[_ <: T]*) extends NullSafeCodec[T] {
  private val nameToVariant = variants.iterator.mkMap(_.caseName, identity)

  private lazy val supportedCasesDebug = variants.map { v =>
    s"${v.caseName} -> ${v.ct.runtimeClass.getName}"
  }.mkString(", ")

  override protected def nullable: Boolean = true

  override protected def readNonNull(input: Input): T = {
    val objectInput = input.readObject()
    val cse = objectInput.nextField().assertField(PolymorphicGenCodec.CaseField).readString()
    val variant = nameToVariant.getOrElse(
      cse,
      throw new IllegalArgumentException(s"Unsupported case marker: $cse. Supported cases are: $supportedCasesDebug")
    )
    variant.readFrom(objectInput)
  }

  override protected def writeNonNull(output: Output, value: T): Unit = {
    val objectOutput = output.writeObject()
    variants.find(_.maybeWriteTo(objectOutput, value)).getOrElse {
      throw new IllegalArgumentException(s"Unsupported object: $value of type ${value.getClass.getName}. Supported cases are: $supportedCasesDebug")
    }
  }
}

object PolymorphicGenCodec {
  private def unsupported = throw new UnsupportedOperationException

  private class CustomObjectInput(objectInput: ObjectInput) extends Input {
    override def readNull(): Null = unsupported
    override def readString(): String = unsupported
    override def readBoolean(): Boolean = unsupported
    override def readInt(): Int = unsupported
    override def readLong(): Long = unsupported
    override def readDouble(): Double = unsupported
    override def readBinary(): Array[Byte] = unsupported
    override def readList(): ListInput = unsupported
    override def skip(): Unit = unsupported

    override def inputType: InputType = InputType.Object
    override def readObject(): ObjectInput = objectInput
  }

  private class CustomObjectOutput(objectOutput: ObjectOutput) extends Output {
    override def writeNull(): Unit = unsupported
    override def writeString(str: String): Unit = unsupported
    override def writeBoolean(boolean: Boolean): Unit = unsupported
    override def writeInt(int: Int): Unit = unsupported
    override def writeLong(long: Long): Unit = unsupported
    override def writeDouble(double: Double): Unit = unsupported
    override def writeBinary(binary: Array[Byte]): Unit = unsupported
    override def writeList(): ListOutput = unsupported

    override def writeObject(): ObjectOutput = objectOutput
  }

  val CaseField = "@case"

  case class Variant[T](caseName: String)(implicit val ct: ClassTag[T], val codec: GenCodec[T]) {
    def readFrom(objectInput: ObjectInput): T = {
      codec.read(new CustomObjectInput(objectInput))
    }

    def maybeWriteTo[U >: T](objectOutput: ObjectOutput, value: U): Boolean = {
      value match {
        case ct(t) =>
          objectOutput.writeField(CaseField).writeString(caseName)
          codec.write(new CustomObjectOutput(objectOutput), t)
          true
        case _ =>
          false
      }
    }
  }
}
