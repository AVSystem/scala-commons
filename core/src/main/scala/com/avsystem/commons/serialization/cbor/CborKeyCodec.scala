package com.avsystem.commons
package serialization.cbor

import com.avsystem.commons.serialization.CustomEventMarker
import com.avsystem.commons.serialization.GenCodec.ReadFailure

/**
  * Custom encoder for CBOR field names. This can be used to encode textual object keys (e.g. case class field
  * names) as arbitrary CBOR data types, e.g. in order to make the final representation more compact, every
  * textual field name may have a numeric label assigned. This numeric label is written as key into the
  * [[CborObjectOutput]] rather than the textual field name.
  */
trait CborKeyCodec {
  def writeFieldKey(fieldName: String, output: CborOutput): Unit
  def readFieldKey(input: CborInput): String
}
object CborKeyCodec {
  final val Default = new CborKeyCodec {
    def writeFieldKey(fieldName: String, output: CborOutput): Unit = output.writeString(fieldName)
    def readFieldKey(input: CborInput): String = input.readString()
  }

  @deprecated("use CborKeyCodec instead", "2.3.0")
  def fromFieldLabels(fieldLabels: FieldLabels): CborKeyCodec =
    new CborKeyCodec {
      def writeFieldKey(fieldName: String, output: CborOutput): Unit =
        fieldLabels.label(fieldName).fold(output.writeString(fieldName))(output.writeInt)

      def readFieldKey(input: CborInput): String = input.readInitialByte().majorType match {
        case MajorType.Unsigned | MajorType.Negative =>
          val label = input.readInt()
          fieldLabels.field(label).getOrElse(throw new ReadFailure(s"unknown CBOR field label: $label"))
        case _ =>
          input.readString()
      }
    }

}

/**
  * Defines translation between textual object field names and corresponding numeric labels. May be used to reduce
  * size of CBOR representation of objects.
  */
@deprecated("use CborKeyCodec instead", "2.3.0")
trait FieldLabels {
  def label(field: String): Opt[Int]
  def field(label: Int): Opt[String]
}
object FieldLabels {
  @deprecated("use CborKeyCodec instead", "2.3.0")
  final val NoLabels: FieldLabels = new FieldLabels {
    def label(field: String): Opt[Int] = Opt.Empty
    def field(label: Int): Opt[String] = Opt.Empty
  }
}

/**
  * Use this with `ObjectOutput.customEvent`/`ObjectInput.customEvent` in order to set custom CBOR key codec
  * for some particular object output or input.
  */
object ForceCborKeyCodec extends CustomEventMarker[CborKeyCodec]
