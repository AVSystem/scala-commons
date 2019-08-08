package com.avsystem.commons
package serialization

import com.avsystem.commons.serialization.GenCodec._

import scala.annotation.tailrec

class SingletonCodec[T <: Singleton](
  protected val typeRepr: String,
  singletonValue: => T
) extends ErrorReportingCodec[T] with OOOFieldsObjectCodec[T] {
  final def nullable = true
  final def readObject(input: ObjectInput, outOfOrderFields: FieldValues): T = singletonValue
  def size(value: T): Int = 0
  def writeFields(output: ObjectOutput, value: T): Unit = ()
}

abstract class ApplyUnapplyCodec[T](
  protected val typeRepr: String,
  val nullable: Boolean,
  fieldNames: Array[String]
) extends ErrorReportingCodec[T] with OOOFieldsObjectCodec[T] {

  protected def dependencies: Array[GenCodec[_]]
  protected def instantiate(fieldValues: FieldValues): T

  private[this] lazy val deps = dependencies

  protected final def writeField[A](output: ObjectOutput, idx: Int, value: A): Unit =
    writeField(fieldNames(idx), output, value, deps(idx).asInstanceOf[GenCodec[A]])

  protected final def writeField[A](output: ObjectOutput, idx: Int, value: A, transient: A): Unit =
    if (value != transient) {
      writeField(output, idx, value)
    }

  protected final def writeField(output: ObjectOutput, idx: Int, value: Boolean): Unit =
    deps(idx) match {
      case GenCodec.BooleanCodec => writeField(fieldNames(idx), output, value)
      case codec: GenCodec[Boolean@unchecked] => writeField(fieldNames(idx), output, value, codec)
    }

  protected final def writeField(output: ObjectOutput, idx: Int, value: Boolean, transient: Boolean): Unit =
    if (value != transient) {
      writeField(output, idx, value)
    }

  protected final def writeField(output: ObjectOutput, idx: Int, value: Int): Unit =
    deps(idx) match {
      case GenCodec.IntCodec => writeField(fieldNames(idx), output, value)
      case codec: GenCodec[Int@unchecked] => writeField(fieldNames(idx), output, value, codec)
    }

  protected final def writeField(output: ObjectOutput, idx: Int, value: Int, transient: Int): Unit =
    if (value != transient) {
      writeField(output, idx, value)
    }

  protected final def writeField(output: ObjectOutput, idx: Int, value: Long): Unit =
    deps(idx) match {
      case GenCodec.LongCodec => writeField(fieldNames(idx), output, value)
      case codec: GenCodec[Long@unchecked] => writeField(fieldNames(idx), output, value, codec)
    }

  protected final def writeField(output: ObjectOutput, idx: Int, value: Long, transient: Long): Unit =
    if (value != transient) {
      writeField(output, idx, value)
    }

  protected final def writeField(output: ObjectOutput, idx: Int, value: Double): Unit =
    deps(idx) match {
      case GenCodec.DoubleCodec => writeField(fieldNames(idx), output, value)
      case codec: GenCodec[Double@unchecked] => writeField(fieldNames(idx), output, value, codec)
    }

  protected final def writeField(output: ObjectOutput, idx: Int, value: Double, transient: Double): Unit =
    if (value != transient) {
      writeField(output, idx, value)
    }

  protected final def getField[A](fieldValues: FieldValues, idx: Int, default: => A): A =
    fieldValues.getOrElse[A](idx, default)

  protected final def getField[A](fieldValues: FieldValues, idx: Int): A =
    fieldValues.getOrElse[A](idx, fieldMissing(fieldNames(idx)))

  final def readObject(input: ObjectInput, outOfOrderFields: FieldValues): T = {
    val fieldValues = new FieldValues(fieldNames, deps, typeRepr)
    fieldValues.rewriteFrom(outOfOrderFields)
    while (input.hasNext) {
      fieldValues.readField(input.nextField())
    }
    instantiate(fieldValues)
  }
}
object ApplyUnapplyCodec {
  def materialize[T]: ApplyUnapplyCodec[T] = macro macros.serialization.GenCodecMacros.applyUnapplyCodec[T]
}

abstract class ProductCodec[T <: Product](
  typeRepr: String,
  nullable: Boolean,
  fieldNames: Array[String]
) extends ApplyUnapplyCodec[T](typeRepr, nullable, fieldNames) {
  def size(value: T): Int = value.productArity

  final def writeFields(output: ObjectOutput, value: T): Unit = {
    val size = value.productArity
    @tailrec def loop(idx: Int): Unit =
      if (idx < size) {
        writeField(output, idx, value.productElement(idx))
        loop(idx + 1)
      }
    loop(0)
  }
}

abstract class SealedHierarchyCodec[T](
  protected val typeRepr: String,
  val nullable: Boolean,
  caseNames: Array[String],
  cases: Array[Class[_]]
) extends ErrorReportingCodec[T] with ObjectCodec[T] {

  @tailrec protected final def caseIndexByValue(value: T, idx: Int = 0): Int =
    if (idx >= cases.length) unknownCase(value)
    else if (cases(idx).isInstance(value)) idx
    else caseIndexByValue(value, idx + 1)

  @tailrec protected final def caseIndexByName(caseName: String, idx: Int = 0): Int =
    if (idx >= caseNames.length) -1
    else if (caseName == caseNames(idx)) idx
    else caseIndexByName(caseName, idx + 1)
}

abstract class NestedSealedHierarchyCodec[T](
  typeRepr: String,
  nullable: Boolean,
  caseNames: Array[String],
  cases: Array[Class[_]]
) extends SealedHierarchyCodec[T](typeRepr, nullable, caseNames, cases) {

  protected def caseDependencies: Array[GenCodec[_]]

  private[this] lazy val caseDeps = caseDependencies

  final def writeObject(output: ObjectOutput, value: T): Unit = {
    val caseIdx = caseIndexByValue(value)
    output.declareSize(1)
    writeCase(caseNames(caseIdx), output, value, caseDeps(caseIdx).asInstanceOf[GenCodec[T]])
  }

  final def readObject(input: ObjectInput): T = {
    if (input.hasNext) {
      val fi = input.nextField()
      val result = caseIndexByName(fi.fieldName) match {
        case -1 => unknownCase(fi.fieldName)
        case idx => readCase(fi.fieldName, fi, caseDeps(idx).asInstanceOf[GenCodec[T]])
      }
      if (input.hasNext) notSingleField(empty = false) else result
    } else notSingleField(empty = true)
  }
}

abstract class FlatSealedHierarchyCodec[T](
  typeRepr: String,
  nullable: Boolean,
  caseNames: Array[String],
  cases: Array[Class[_]],
  oooFieldNames: Array[String],
  caseDependentFieldNames: Set[String],
  override protected val caseFieldName: String,
  defaultCaseIdx: Int,
  defaultCaseTransient: Boolean
) extends SealedHierarchyCodec[T](typeRepr, nullable, caseNames, cases) {

  protected def oooDependencies: Array[GenCodec[_]]
  protected def caseDependencies: Array[OOOFieldsObjectCodec[_]]

  private[this] lazy val oooDeps = oooDependencies
  private[this] lazy val caseDeps = caseDependencies

  final def writeObject(output: ObjectOutput, value: T): Unit = {
    val caseIdx = caseIndexByValue(value)
    val transient = defaultCaseTransient && defaultCaseIdx == caseIdx
    val caseCodec = caseDeps(caseIdx).asInstanceOf[OOOFieldsObjectCodec[T]]
    if (output.sizePolicy != SizePolicy.Ignored) {
      output.declareSize((if (transient) 0 else 1) + caseCodec.size(value))
    }
    writeFlatCase(caseNames(caseIdx), transient, output, value, caseCodec)
  }

  final def readObject(input: ObjectInput): T = {
    val oooFields = new FieldValues(oooFieldNames, oooDeps, typeRepr)

    def readCase(caseNameField: FieldInput): T = {
      val caseName = readCaseName(caseNameField)
      caseIndexByName(caseName) match {
        case -1 => unknownCase(caseName)
        case idx => readFlatCase(caseName, oooFields, input,
          caseDeps(idx).asInstanceOf[OOOFieldsObjectCodec[T]])
      }
    }

    @tailrec def read(): T =
      if (input.hasNext) {
        val fi = input.nextField()
        if (fi.fieldName == caseFieldName) readCase(fi)
        else if (!oooFields.tryReadField(fi)) {
          if (caseDependentFieldNames.contains(fi.fieldName)) {
            if (defaultCaseIdx != -1) {
              val defaultCaseName = caseNames(defaultCaseIdx)
              val wrappedInput = new DefaultCaseObjectInput(fi, input, defaultCaseName)
              readFlatCase(defaultCaseName, oooFields, wrappedInput,
                caseDeps(defaultCaseIdx).asInstanceOf[OOOFieldsObjectCodec[T]])
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
        readFlatCase(caseNames(defaultCaseIdx), oooFields, input,
          caseDeps(defaultCaseIdx).asInstanceOf[OOOFieldsObjectCodec[T]])
      } else {
        missingCase
      }

    input.peekField(caseFieldName) match {
      case Opt(fi) => readCase(fi)
      case Opt.Empty => read()
    }
  }
}

abstract class ErrorReportingCodec[T] extends GenCodec[T] {
  protected def typeRepr: String
  protected def caseFieldName: String = DefaultCaseField

  protected final def readField[A](fieldInput: FieldInput, codec: GenCodec[A]): A =
    try codec.read(fieldInput) catch {
      case NonFatal(e) => throw FieldReadFailed(typeRepr, fieldInput.fieldName, e)
    }

  protected final def readCaseName(fi: FieldInput): String =
    try fi.readSimple().readString() catch {
      case NonFatal(e) => throw FieldReadFailed(typeRepr, fi.fieldName, e)
    }

  protected final def readCase[A](caseName: String, input: Input, codec: GenCodec[A]): A =
    try codec.read(input) catch {
      case NonFatal(e) => throw CaseReadFailed(typeRepr, caseName, e)
    }

  protected final def readFlatCase[A](caseName: String, outOfOrderFields: FieldValues, input: ObjectInput, codec: OOOFieldsObjectCodec[A]): A =
    try codec.readObject(input, outOfOrderFields) catch {
      case NonFatal(e) => throw CaseReadFailed(typeRepr, caseName, e)
    }

  protected final def readFlatCase[A](caseName: String, input: ObjectInput, codec: GenCodec.ObjectCodec[A]): A =
    try codec.readObject(input) catch {
      case NonFatal(e) => throw CaseReadFailed(typeRepr, caseName, e)
    }

  protected final def writeField[A](fieldName: String, output: ObjectOutput, value: A, codec: GenCodec[A]): Unit =
    try codec.write(output.writeField(fieldName), value) catch {
      case NonFatal(e) => throw FieldWriteFailed(typeRepr, fieldName, e)
    }

  protected final def writeField(fieldName: String, output: ObjectOutput, value: Boolean): Unit =
    try output.writeField(fieldName).writeSimple().writeBoolean(value) catch {
      case NonFatal(e) => throw FieldWriteFailed(typeRepr, fieldName, e)
    }

  protected final def writeField(fieldName: String, output: ObjectOutput, value: Int): Unit =
    try output.writeField(fieldName).writeSimple().writeInt(value) catch {
      case NonFatal(e) => throw FieldWriteFailed(typeRepr, fieldName, e)
    }

  protected final def writeField(fieldName: String, output: ObjectOutput, value: Long): Unit =
    try output.writeField(fieldName).writeSimple().writeLong(value) catch {
      case NonFatal(e) => throw FieldWriteFailed(typeRepr, fieldName, e)
    }

  protected final def writeField(fieldName: String, output: ObjectOutput, value: Double): Unit =
    try output.writeField(fieldName).writeSimple().writeDouble(value) catch {
      case NonFatal(e) => throw FieldWriteFailed(typeRepr, fieldName, e)
    }

  protected final def writeCase[A](caseName: String, output: ObjectOutput, value: A, codec: GenCodec[A]): Unit =
    try codec.write(output.writeField(caseName), value) catch {
      case NonFatal(e) => throw CaseWriteFailed(typeRepr, caseName, e)
    }

  protected final def writeFlatCase[A](
    caseName: String, transient: Boolean, output: ObjectOutput, value: A, codec: OOOFieldsObjectCodec[A]
  ): Unit = try {
    if (!transient) {
      output.writeField(caseFieldName).writeSimple().writeString(caseName)
    }
    codec.writeFields(output, value)
  } catch {
    case NonFatal(e) => throw CaseWriteFailed(typeRepr, caseName, e)
  }

  protected final def unknownCase(value: T) =
    throw UnknownWrittenCase(typeRepr, value)

  protected final def fieldMissing(field: String) =
    throw MissingField(typeRepr, field)

  protected final def unknownCase(caseName: String) =
    throw UnknownCase(typeRepr, caseName)

  protected final def missingCase(fieldToRead: String) =
    throw MissingCase(typeRepr, caseFieldName, Opt(fieldToRead))

  protected final def missingCase =
    throw MissingCase(typeRepr, caseFieldName, Opt.Empty)

  protected final def notSingleField(empty: Boolean) =
    throw NotSingleField(typeRepr, empty)

  protected final def unapplyFailed =
    throw UnapplyFailed(typeRepr)
}
