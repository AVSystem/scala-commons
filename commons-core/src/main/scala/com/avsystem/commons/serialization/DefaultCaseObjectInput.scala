package com.avsystem.commons
package serialization

import com.avsystem.commons.serialization.GenCodec.ReadFailure

final class DefaultCaseObjectInput(firstField: FieldInput, actualInput: ObjectInput, caseFieldName: String)
  extends ObjectInput {

  private[this] var atFirstField = true

  def hasNext = atFirstField || actualInput.hasNext
  def nextField() =
    if (atFirstField) {
      atFirstField = false
      firstField
    } else {
      val field = actualInput.nextField()
      if (field.fieldName == caseFieldName) {
        throw new ReadFailure(s"$caseFieldName field found too far into the object")
      }
      field
    }
}
