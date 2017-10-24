package com.avsystem.commons
package serialization

import com.avsystem.commons.serialization.GenCodec.{DefaultCaseField, NullSafeCodec, OOOFieldsObjectCodec, ObjectCodec, ReadFailure, WriteFailure}

import scala.annotation.tailrec

class SingletonCodec[T <: Singleton](
  protected val typeRepr: String,
  singletonValue: => T
) extends OOOFieldsObjectCodec[T] with ErrorReportingCodec[T] {
  final def nullable = true
  final def readObject(input: ObjectInput, outOfOrderFields: FieldValues) = singletonValue
  def writeObject(output: ObjectOutput, value: T): Unit = ()
}

abstract class ApplyUnapplyCodec[T](
  protected val typeRepr: String,
  val nullable: Boolean,
  fieldNames: Array[String]
) extends OOOFieldsObjectCodec[T] with ErrorReportingCodec[T] {
  protected def dependencies: Array[GenCodec[_]]
  protected def instantiate(fieldValues: FieldValues): T

  protected final def writeField[A](output: ObjectOutput, idx: Int, value: A): Unit =
    writeField(fieldNames(idx), output, value, dependencies(idx).asInstanceOf[GenCodec[A]])

  protected final def getField[A](fieldValues: FieldValues, idx: Int, default: => A): A =
    fieldValues.getOrElse[A](idx, default)

  protected final def getField[A](fieldValues: FieldValues, idx: Int): A =
    fieldValues.getOrElse[A](idx, fieldMissing(fieldNames(idx)))

  final def readObject(input: ObjectInput, outOfOrderFields: FieldValues): T = {
    val fieldValues = new FieldValues(fieldNames, dependencies)
    fieldValues.rewriteFrom(outOfOrderFields)
    while (input.hasNext) {
      fieldValues.readField(input.nextField())
    }
    instantiate(fieldValues)
  }
}

abstract class TransparentCodec[T, U](
  protected val typeRepr: String,
  val nullable: Boolean,
  underlyingCodec: GenCodec[U]
) extends NullSafeCodec[T] with ErrorReportingCodec[T] {
  protected def wrap(underlying: U): T
  protected def unwrap(value: T): U

  final def readNonNull(input: Input): T = wrap(underlyingCodec.read(input))
  final def writeNonNull(output: Output, value: T): Unit = underlyingCodec.write(output, unwrap(value))
}

abstract class SealedHierarchyCodec[T](
  protected val typeRepr: String,
  val nullable: Boolean,
  caseNames: Array[String]
) extends ObjectCodec[T] with ErrorReportingCodec[T] {

  @tailrec protected final def caseIndex(caseName: String, idx: Int): Int =
    if (idx >= caseNames.length) -1
    else if (caseName == caseNames(idx)) idx
    else caseIndex(caseName, idx + 1)
}

abstract class NestedSealedHierarchyCodec[T](
  typeRepr: String,
  nullable: Boolean,
  caseNames: Array[String]
) extends SealedHierarchyCodec[T](typeRepr, nullable, caseNames) {

  def caseDependencies: Array[GenCodec[_ <: T]]

  protected final def writeCase[A <: T](output: ObjectOutput, idx: Int, value: A): Unit =
    writeCase(caseNames(idx), output, value, caseDependencies(idx).asInstanceOf[GenCodec[A]])

  final def readObject(input: ObjectInput): T = {
    if (input.hasNext) {
      val fi = input.nextField()
      val result = caseIndex(fi.fieldName, 0) match {
        case -1 => unknownCase(fi.fieldName)
        case idx => readCase(fi.fieldName, fi, caseDependencies(idx))
      }
      if (input.hasNext) notSingleField(empty = false) else result
    } else notSingleField(empty = true)
  }
}

abstract class FlatSealedHierarchyCodec[T](
  typeRepr: String,
  nullable: Boolean,
  caseNames: Array[String],
  oooFieldNames: Array[String],
  caseDependentFieldNames: Set[String],
  override protected val caseFieldName: String,
  defaultCaseIdx: Int
) extends SealedHierarchyCodec[T](typeRepr, nullable, caseNames) {

  def oooDependencies: Array[GenCodec[_]]
  def caseDependencies: Array[OOOFieldsObjectCodec[_ <: T]]

  protected final def writeCase[A <: T](output: ObjectOutput, idx: Int, transient: Boolean, value: A): Unit =
    writeFlatCase(caseNames(idx), transient, output, value, caseDependencies(idx).asInstanceOf[OOOFieldsObjectCodec[A]])

  final def readObject(input: ObjectInput): T = {
    val oooFields = new FieldValues(oooFieldNames, oooDependencies)

    def read(): T =
      if (input.hasNext) {
        val fi = input.nextField()
        if (fi.fieldName == caseFieldName) {
          val caseName = readCaseName(fi)
          caseIndex(caseName, 0) match {
            case -1 => unknownCase(caseName)
            case idx => readFlatCase(caseName, oooFields, input, caseDependencies(idx))
          }
        } else if (!oooFields.tryReadField(fi)) {
          if (caseDependentFieldNames.contains(fi.fieldName)) {
            if (defaultCaseIdx != -1) {
              val defaultCaseName = caseNames(defaultCaseIdx)
              val wrappedInput = new DefaultCaseObjectInput(fi, input, defaultCaseName)
              readFlatCase(defaultCaseName, oooFields, wrappedInput, caseDependencies(defaultCaseIdx))
            } else {
              missingCase(fi.fieldName)
            }
          } else {
            fi.skip()
            read()
          }
        } else {
          read()
        }
      } else if (defaultCaseIdx != -1) {
        readFlatCase(caseNames(defaultCaseIdx), oooFields, input, caseDependencies(defaultCaseIdx))
      } else {
        missingCase
      }

    read()
  }
}

trait ErrorReportingCodec[T] extends GenCodec[T] {
  protected def typeRepr: String
  protected def caseFieldName: String = DefaultCaseField

  protected def readField[A](fieldInput: FieldInput, codec: GenCodec[A]): A =
    decoratedRead(fieldInput, codec, "field", fieldInput.fieldName)

  protected def readCaseName(fi: FieldInput): String =
    try fi.readString() catch {
      case NonFatal(e) =>
        throw new ReadFailure(s"Cannot read $typeRepr, failed to read case name from $caseFieldName field", e)
    }

  protected def readCase[A](caseName: String, input: Input, codec: GenCodec[A]): A =
    decoratedRead(input, codec, "case", caseName)

  protected def readFlatCase[A](caseName: String, outOfOrderFields: FieldValues, input: ObjectInput, codec: OOOFieldsObjectCodec[A]): A =
    try codec.readObject(input, outOfOrderFields) catch {
      case NonFatal(e) => throw new ReadFailure(s"Failed to read case $caseName of $typeRepr", e)
    }

  protected def readFlatCase[A](caseName: String, input: ObjectInput, codec: GenCodec.ObjectCodec[A]): A =
    try codec.readObject(input) catch {
      case NonFatal(e) => throw new ReadFailure(s"Failed to read case $caseName of $typeRepr", e)
    }

  private def decoratedRead[A](input: Input, codec: GenCodec[A], what: String, name: String): A =
    try codec.read(input) catch {
      case NonFatal(e) => throw new ReadFailure(s"Failed to read $what $name of $typeRepr", e)
    }

  protected def writeField[A](fieldName: String, output: ObjectOutput, value: A, codec: GenCodec[A]): Unit =
    decoratedWrite(fieldName, output, value, codec, "field")

  protected def writeCase[A](fieldName: String, output: ObjectOutput, value: A, codec: GenCodec[A]): Unit =
    decoratedWrite(fieldName, output, value, codec, "case")

  protected def writeFlatCase[A](caseName: String, transient: Boolean, output: ObjectOutput, value: A, codec: ObjectCodec[A]): Unit =
    try {
      if (!transient) {
        output.writeField(caseFieldName).writeString(caseName)
      }
      codec.writeObject(output, value)
    } catch {
      case NonFatal(e) => throw new WriteFailure(s"Failed to write case $caseName of $typeRepr", e)
    }

  private def decoratedWrite[A](fieldName: String, output: ObjectOutput, value: A, codec: GenCodec[A], what: String): Unit =
    try codec.write(output.writeField(fieldName), value) catch {
      case NonFatal(e) => throw new WriteFailure(s"Failed to write $what $fieldName of $typeRepr", e)
    }

  protected def fieldMissing(field: String) =
    throw new ReadFailure(s"Cannot read $typeRepr, field $field is missing in decoded data")

  protected def unknownCase(caseName: String) =
    throw new ReadFailure(s"Cannot read $typeRepr, unknown case: $caseName")

  protected def missingCase(fieldToRead: String) =
    throw new ReadFailure(s"Cannot read field $fieldToRead of $typeRepr before $caseFieldName field is read")

  protected def missingCase =
    throw new ReadFailure(s"Cannot read $typeRepr, $caseFieldName field is missing")

  protected def notSingleField(empty: Boolean) =
    throw new ReadFailure(s"Cannot read $typeRepr, expected object with exactly one field but got ${if (empty) "empty object" else "more than one"}")

  protected def unapplyFailed =
    throw new WriteFailure(s"Could not write $typeRepr, unapply/unapplySeq returned false or empty value")
}