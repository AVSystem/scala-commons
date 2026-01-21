package com.avsystem.commons
package serialization

/** Wrapper over [[ObjectInput]] that lets you peek next field name without advancing the input.
  */
final class PeekingObjectInput(original: ObjectInput) extends ObjectInput {
  private var peekedField: FieldInput | Null = scala.compiletime.uninitialized

  override def knownSize: Int = original.knownSize

  def peekNextFieldName: Opt[String] = peekedField match {
    case null if original.hasNext =>
      peekedField = original.nextField()
      peekedField.nn.fieldName.opt
    case null => Opt.Empty
    case fi => fi.fieldName.opt
  }

  def nextField(): FieldInput =
    peekedField match {
      case null => original.nextField()
      case fi =>
        peekedField = null
        fi
    }

  def hasNext: Boolean =
    peekedField != null || original.hasNext

  override def peekField(name: String): Opt[FieldInput] =
    original.peekField(name)
}
