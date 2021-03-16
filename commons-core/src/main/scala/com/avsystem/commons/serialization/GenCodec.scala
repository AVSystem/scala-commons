package com.avsystem.commons
package serialization

import java.util.UUID

import com.avsystem.commons.annotation.explicitGenerics
import com.avsystem.commons.derivation.{AllowImplicitMacro, DeferredInstance}
import com.avsystem.commons.jiop.JFactory
import com.avsystem.commons.meta.Fallback
import com.avsystem.commons.misc.{Bytes, Timestamp}

import scala.annotation.{implicitNotFound, tailrec}
import scala.collection.compat._

/**
  * Type class for types that can be serialized to [[Output]] (format-agnostic "output stream") and deserialized
  * from [[Input]] (format-agnostic "input stream"). `GenCodec` is supposed to capture generic structure of serialized
  * objects, without being bound to particular format like JSON. The actual format is determined by implementation
  * of [[Input]] and [[Output]].
  *
  * There are convenient macros for automatic derivation of [[GenCodec]] instances (`materialize` and `materializeRecursively`).
  * However, [[GenCodec]] instances still need to be explicitly declared and won't be derived "automagically".
  */
@implicitNotFound("No GenCodec found for ${T}")
trait GenCodec[T] {
  /**
    * Deserializes a value of type `T` from an [[Input]].
    */
  def read(input: Input): T

  /**
    * Serializes a value of type `T` into an [[Output]].
    */
  def write(output: Output, value: T): Unit

  /**
    * Transforms this codec into a codec of other type using a bidirectional conversion
    * between the original and new type.
    */
  final def transform[U](onWrite: U => T, onRead: T => U): GenCodec[U] =
    new GenCodec.Transformed[U, T](this, onWrite, onRead)
}

object GenCodec extends RecursiveAutoCodecs with TupleGenCodecs {
  def apply[T](implicit codec: GenCodec[T]): GenCodec[T] = codec

  /**
    * Macro that automatically materializes a [[GenCodec]] for some type `T`, which must be one of:
    * <ul>
    * <li>singleton type, e.g. an `object`</li>
    * <li>case class whose every field type has its own [[GenCodec]]</li>
    * <li>(generalization of case classes) class or trait whose companion object has a pair of case-class-like `apply`
    * and `unapply` methods and every parameter type of `apply` method has its own [[GenCodec]]
    * </li>
    * <li>sealed hierarchy in which every non-abstract subclass either has its own [[GenCodec]] or it can be
    * automatically materialized with the same mechanism</li>
    * </ul>
    * Note that automatic materialization does NOT descend into types that `T` is made of (e.g. types of case class
    * fields must have their own codecs independently declared). If you want recursive materialization, use
    * `materializeRecursively`.
    */
  def materialize[T]: GenCodec[T] = macro macros.serialization.GenCodecMacros.materialize[T]

  /**
    * Materializes a [[GenCodec]] for type `T` using `apply` and `unapply`/`unapplySeq` methods available on
    * passed `applyUnapplyProvider` object. The signatures of `apply` and `unapply` must be as if `T` was a case class
    * and `applyUnapplyProvider` was its companion object.
    * This is useful for easy derivation of [[GenCodec]] for third party classes which don't have their own companion
    * objects with `apply` and `unapply`. So essentially the `applyUnapplyProvider` is a "fake companion object"
    * of type `T`.
    *
    * Example:
    * {{{
    *   class ThirdParty { ... }
    *
    *   object ThirdPartyFakeCompanion {
    *     def apply(int: Int, string: String): ThirdParty = ...
    *     def unapply(tp: ThirdParty): Option[(Int, String)] = ...
    *   }
    *
    *   implicit val thirdPartyCodec: GenCodec[ThirdParty] =
    *     GenCodec.fromApplyUnapplyProvider[ThirdParty](ThirdPartyFakeCompanion)
    * }}}
    */
  def fromApplyUnapplyProvider[T](applyUnapplyProvider: Any): GenCodec[T] =
  macro macros.serialization.GenCodecMacros.fromApplyUnapplyProvider[T]

  def applyUnapplyCodec[T]: ApplyUnapplyCodec[T] =
  macro macros.serialization.GenCodecMacros.applyUnapplyCodec[T]

  /**
    * Materializes a [[GenCodec]] for a POJO that has a fluent builder. The fluent builder must have setters
    * corresponding to the POJO's getters. Each setter must return the builder itself (because it's fluent).
    * The builder is assumed to have default value for each field. These values are considered "transient", i.e.
    * the codec will omit them during serialization, similarly to [[transientDefault]] annotation in case classes.
    *
    * @param newBuilder an expression that creates a fresh builder
    * @param build      a function that builds the final value (typically `_.build()` or `_.get()`)
    */
  def fromJavaBuilder[T, B](newBuilder: => B)(build: B => T): GenCodec[T] =
  macro macros.serialization.GenCodecMacros.fromJavaBuilder[T, B]

  @explicitGenerics
  def read[T: GenCodec](input: Input): T =
    apply[T].read(input)

  def write[T: GenCodec](output: Output, value: T): Unit =
    apply[T].write(output, value)

  def create[T](readFun: Input => T, writeFun: (Output, T) => Any): GenCodec[T] =
    new GenCodec[T] {
      def write(output: Output, value: T): Unit = writeFun(output, value)
      def read(input: Input): T = readFun(input)
    }

  def makeLazy[T](codec: => GenCodec[T]): GenCodec[T] = new GenCodec[T] {
    private lazy val underlying = codec
    def read(input: Input): T = underlying.read(input)
    def write(output: Output, value: T): Unit = underlying.write(output, value)
  }

  def transformed[T, R: GenCodec](toRaw: T => R, fromRaw: R => T): GenCodec[T] =
    new Transformed[T, R](GenCodec[R], toRaw, fromRaw)

  def nullSafe[T](readFun: Input => T, writeFun: (Output, T) => Any, allowNull: Boolean): GenCodec[T] =
    new NullSafeCodec[T] {
      def nullable: Boolean = allowNull
      def readNonNull(input: Input): T = readFun(input)
      def writeNonNull(output: Output, value: T): Unit = writeFun(output, value)
    }

  def nullable[T <: AnyRef](readFun: Input => T, writeFun: (Output, T) => Any): GenCodec[T] =
    nullSafe(readFun, writeFun, allowNull = true)

  def nonNull[T](readFun: Input => T, writeFun: (Output, T) => Any): GenCodec[T] =
    nullSafe(readFun, writeFun, allowNull = false)

  def nonNullString[T](readFun: String => T, writeFun: T => String): GenCodec[T] =
    nonNullSimple(i => readFun(i.readString()), (o, v) => o.writeString(writeFun(v)))

  def nullableString[T <: AnyRef](readFun: String => T, writeFun: T => String): GenCodec[T] =
    nullableSimple(i => readFun(i.readString()), (o, v) => o.writeString(writeFun(v)))

  def createSimple[T](readFun: SimpleInput => T, writeFun: (SimpleOutput, T) => Any, allowNull: Boolean): GenCodec[T] =
    new SimpleCodec[T] {
      def nullable: Boolean = allowNull
      def readSimple(input: SimpleInput): T = readFun(input)
      def writeSimple(output: SimpleOutput, value: T): Unit = writeFun(output, value)
    }

  def nullableSimple[T <: AnyRef](readFun: SimpleInput => T, writeFun: (SimpleOutput, T) => Any): GenCodec[T] =
    createSimple(readFun, writeFun, allowNull = true)

  def nonNullSimple[T](readFun: SimpleInput => T, writeFun: (SimpleOutput, T) => Any): GenCodec[T] =
    createSimple(readFun, writeFun, allowNull = false)

  def createList[T](readFun: ListInput => T, writeFun: (ListOutput, T) => Any, allowNull: Boolean): GenCodec[T] =
    new ListCodec[T] {
      def nullable: Boolean = allowNull
      def readList(input: ListInput): T = readFun(input)
      def writeList(output: ListOutput, value: T): Unit = writeFun(output, value)
    }

  def nullableList[T <: AnyRef](readFun: ListInput => T, writeFun: (ListOutput, T) => Any): GenCodec[T] =
    createList(readFun, writeFun, allowNull = true)

  def nonNullList[T](readFun: ListInput => T, writeFun: (ListOutput, T) => Any): GenCodec[T] =
    createList(readFun, writeFun, allowNull = false)

  /**
    * Helper method to manually implement a `GenCodec` that writes an object. NOTE: in most cases the easiest way to
    * have a custom object codec is to manually implement `apply` and `unapply`/`unapplySeq` methods in companion object
    * of your type or use [[fromApplyUnapplyProvider]] if the type comes from a third party code and you can't
    * modify its companion object.
    */
  def createObject[T](readFun: ObjectInput => T, writeFun: (ObjectOutput, T) => Any, allowNull: Boolean): GenObjectCodec[T] =
    new ObjectCodec[T] {
      def nullable: Boolean = allowNull
      def readObject(input: ObjectInput): T = readFun(input)
      def writeObject(output: ObjectOutput, value: T): Unit = writeFun(output, value)
    }

  def nullableObject[T <: AnyRef](readFun: ObjectInput => T, writeFun: (ObjectOutput, T) => Any): GenObjectCodec[T] =
    createObject(readFun, writeFun, allowNull = true)

  def nonNullObject[T](readFun: ObjectInput => T, writeFun: (ObjectOutput, T) => Any): GenObjectCodec[T] =
    createObject(readFun, writeFun, allowNull = false)

  def fromKeyCodec[T](implicit keyCodec: GenKeyCodec[T]): GenCodec[T] = create(
    input => keyCodec.read(input.readSimple().readString()),
    (output, value) => output.writeSimple().writeString(keyCodec.write(value))
  )

  def forSealedEnum[T]: GenCodec[T] = macro macros.serialization.GenCodecMacros.forSealedEnum[T]

  class ReadFailure(msg: String, cause: Throwable) extends RuntimeException(msg, cause) {
    def this(msg: String) = this(msg, null)

    override def fillInStackTrace(): Throwable =
      if (cause == null) super.fillInStackTrace() else this
  }

  case class MissingField(typeRepr: String, fieldName: String)
    extends ReadFailure(s"Cannot read $typeRepr, field $fieldName is missing in decoded data")
  case class UnknownCase(typeRepr: String, caseName: String)
    extends ReadFailure(s"Cannot read $typeRepr, unknown case: $caseName")
  case class MissingCase(typeRepr: String, caseFieldName: String, fieldToRead: Opt[String])
    extends ReadFailure(fieldToRead match {
      case Opt(fr) => s"Cannot read field $fr of $typeRepr before $caseFieldName field is read"
      case Opt.Empty => s"Cannot read $typeRepr, $caseFieldName field is missing"
    })
  case class NotSingleField(typeRepr: String, empty: Boolean)
    extends ReadFailure(s"Cannot read $typeRepr, expected object with exactly one field but got " +
      (if (empty) "empty object" else "more than one"))
  case class CaseReadFailed(typeRepr: String, caseName: String, cause: Throwable)
    extends ReadFailure(s"Failed to read case $caseName of $typeRepr", cause)
  case class FieldReadFailed(typeRepr: String, fieldName: String, cause: Throwable)
    extends ReadFailure(s"Failed to read field $fieldName of $typeRepr", cause)
  case class ListElementReadFailed(idx: Int, cause: Throwable)
    extends ReadFailure(s"Failed to read list element at index $idx", cause)
  case class MapFieldReadFailed(fieldName: String, cause: Throwable)
    extends ReadFailure(s"Failed to read map field $fieldName", cause)

  class WriteFailure(msg: String, cause: Throwable) extends RuntimeException(msg, cause) {
    def this(msg: String) = this(msg, null)

    override def fillInStackTrace(): Throwable =
      if (cause == null) super.fillInStackTrace() else this
  }

  case class UnknownWrittenCase[T](typeRepr: String, value: T)
    extends WriteFailure(s"Failed to write $typeRepr: value $value does not match any of known subtypes")
  case class UnapplyFailed(typeRepr: String)
    extends WriteFailure(s"Could not write $typeRepr, unapply/unapplySeq returned false or empty value")
  case class CaseWriteFailed(typeRepr: String, caseName: String, cause: Throwable)
    extends WriteFailure(s"Failed to write case $caseName of $typeRepr", cause)
  case class FieldWriteFailed(typeRepr: String, fieldName: String, cause: Throwable)
    extends WriteFailure(s"Failed to write field $fieldName of $typeRepr", cause)
  case class ListElementWriteFailed(idx: Int, cause: Throwable)
    extends WriteFailure(s"Failed to write list element at index $idx", cause)
  case class MapFieldWriteFailed(fieldName: String, cause: Throwable)
    extends WriteFailure(s"Failed to write map field $fieldName", cause)

  final class Deferred[T] extends DeferredInstance[GenCodec[T]] with GenCodec[T] {
    def read(input: Input): T = underlying.read(input)
    def write(output: Output, value: T): Unit = underlying.write(output, value)
  }

  trait NullSafeCodec[T] extends GenCodec[T] {
    def nullable: Boolean
    def readNonNull(input: Input): T
    def writeNonNull(output: Output, value: T): Unit

    final override def write(output: Output, value: T): Unit =
      if (value == null)
        if (nullable) output.writeNull() else throw new WriteFailure("null")
      else writeNonNull(output, value)

    final override def read(input: Input): T =
      if (input.readNull())
        if (nullable) null.asInstanceOf[T] else throw new ReadFailure("null")
      else readNonNull(input)
  }

  trait SimpleCodec[T] extends NullSafeCodec[T] {
    def readSimple(input: SimpleInput): T
    def writeSimple(output: SimpleOutput, value: T): Unit

    final def writeNonNull(output: Output, value: T): Unit =
      writeSimple(output.writeSimple(), value)

    final def readNonNull(input: Input): T =
      readSimple(input.readSimple())
  }

  trait ListCodec[T] extends NullSafeCodec[T] {
    def readList(input: ListInput): T
    def writeList(output: ListOutput, value: T): Unit

    final def writeNonNull(output: Output, value: T): Unit = {
      val lo = output.writeList()
      writeList(lo, value)
      lo.finish()
    }
    final def readNonNull(input: Input): T = {
      val li = input.readList()
      val result = readList(li)
      li.skipRemaining()
      result
    }
  }

  /**
    * Convenience base class for `GenCodec`s that serialize values as objects.
    * NOTE: if you need to implement a custom `GenCodec` that writes an object, the best way to do it is to have
    * manually implemented `apply` and `unapply` in companion object or by using [[GenCodec.fromApplyUnapplyProvider]].
    */
  trait ObjectCodec[T] extends GenObjectCodec[T] with NullSafeCodec[T] {
    def readObject(input: ObjectInput): T
    def writeObject(output: ObjectOutput, value: T): Unit

    final def writeNonNull(output: Output, value: T): Unit = {
      val oo = output.writeObject()
      writeObject(oo, value)
      oo.finish()
    }
    final def readNonNull(input: Input): T = {
      val oi = input.readObject()
      val result = readObject(oi)
      oi.skipRemaining()
      result
    }
  }

  trait OOOFieldsObjectCodec[T] extends ObjectCodec[T] {
    def size(value: T): Int

    protected final def declareSizeFor(output: ObjectOutput, value: T): Unit =
      if (output.sizePolicy != SizePolicy.Ignored) {
        output.declareSize(size(value))
      }

    def readObject(input: ObjectInput, outOfOrderFields: FieldValues): T
    def writeFields(output: ObjectOutput, value: T): Unit

    final def readObject(input: ObjectInput): T =
      readObject(input, FieldValues.Empty)

    final def writeObject(output: ObjectOutput, value: T): Unit = {
      declareSizeFor(output, value)
      writeFields(output, value)
    }
  }

  final class Transformed[A, B](val wrapped: GenCodec[B], onWrite: A => B, onRead: B => A) extends GenCodec[A] {
    def read(input: Input): A = {
      val wrappedValue = wrapped.read(input)
      try onRead(wrappedValue) catch {
        case NonFatal(cause) => throw new ReadFailure(s"onRead conversion failed", cause)
      }
    }

    def write(output: Output, value: A): Unit = {
      val wrappedValue = try onWrite(value) catch {
        case NonFatal(cause) => throw new WriteFailure(s"onWrite conversion failed", cause)
      }
      wrapped.write(output, wrappedValue)
    }
  }

  def underlyingCodec(codec: GenCodec[_]): GenCodec[_] = codec match {
    case tc: Transformed[_, _] => underlyingCodec(tc.wrapped)
    case _ => codec
  }

  class SubclassCodec[T: ClassTag, S >: T : GenCodec](val nullable: Boolean) extends NullSafeCodec[T] {
    override def readNonNull(input: Input): T = GenCodec.read[S](input) match {
      case sub: T => sub
      case v => throw new ReadFailure(s"$v is not an instance of ${classTag[T].runtimeClass}")
    }
    override def writeNonNull(output: Output, value: T): Unit = GenCodec.write[S](output, value)
  }

  final val DefaultCaseField = "_case"

  private def notNull = throw new ReadFailure("not null")

  implicit lazy val NothingCodec: GenCodec[Nothing] =
    create[Nothing](_ => throw new ReadFailure("read Nothing"), (_, _) => throw new WriteFailure("write Nothing"))
  implicit lazy val NullCodec: GenCodec[Null] =
    create[Null](i => if (i.readNull()) null else notNull, (o, _) => o.writeNull())
  implicit lazy val UnitCodec: GenCodec[Unit] =
    create[Unit](i => if (i.readNull()) () else notNull, (o, _) => o.writeNull())
  implicit lazy val VoidCodec: GenCodec[Void] =
    create[Void](i => if (i.readNull()) null else notNull, (o, _) => o.writeNull())

  implicit lazy val BooleanCodec: GenCodec[Boolean] = nonNullSimple(_.readBoolean(), _ writeBoolean _)
  implicit lazy val CharCodec: GenCodec[Char] = nonNullSimple(_.readChar(), _ writeChar _)
  implicit lazy val ByteCodec: GenCodec[Byte] = nonNullSimple(_.readByte(), _ writeByte _)
  implicit lazy val ShortCodec: GenCodec[Short] = nonNullSimple(_.readShort(), _ writeShort _)
  implicit lazy val IntCodec: GenCodec[Int] = nonNullSimple(_.readInt(), _ writeInt _)
  implicit lazy val LongCodec: GenCodec[Long] = nonNullSimple(_.readLong(), _ writeLong _)
  implicit lazy val FloatCodec: GenCodec[Float] = nonNullSimple(_.readFloat(), _ writeFloat _)
  implicit lazy val DoubleCodec: GenCodec[Double] = nonNullSimple(_.readDouble(), _ writeDouble _)
  implicit lazy val BigIntCodec: GenCodec[BigInt] = nullableSimple(_.readBigInt(), _ writeBigInt _)
  implicit lazy val BigDecimalCodec: GenCodec[BigDecimal] = nullableSimple(_.readBigDecimal(), _ writeBigDecimal _)

  implicit lazy val JBooleanCodec: GenCodec[JBoolean] = nullableSimple(_.readBoolean(), _ writeBoolean _)
  implicit lazy val JCharacterCodec: GenCodec[JCharacter] = nullableSimple(_.readChar(), _ writeChar _)
  implicit lazy val JByteCodec: GenCodec[JByte] = nullableSimple(_.readByte(), _ writeByte _)
  implicit lazy val JShortCodec: GenCodec[JShort] = nullableSimple(_.readShort(), _ writeShort _)
  implicit lazy val JIntegerCodec: GenCodec[JInteger] = nullableSimple(_.readInt(), _ writeInt _)
  implicit lazy val JLongCodec: GenCodec[JLong] = nullableSimple(_.readLong(), _ writeLong _)
  implicit lazy val JFloatCodec: GenCodec[JFloat] = nullableSimple(_.readFloat(), _ writeFloat _)
  implicit lazy val JDoubleCodec: GenCodec[JDouble] = nullableSimple(_.readDouble(), _ writeDouble _)
  implicit lazy val JBigIntegerCodec: GenCodec[JBigInteger] =
    nullableSimple(_.readBigInt().bigInteger, (o, v) => o.writeBigInt(BigInt(v)))
  implicit lazy val JBigDecimalCodec: GenCodec[JBigDecimal] =
    nullableSimple(_.readBigDecimal().bigDecimal, (o, v) => o.writeBigDecimal(BigDecimal(v)))

  implicit lazy val JDateCodec: GenCodec[JDate] =
    nullableSimple(i => new JDate(i.readTimestamp()), (o, d) => o.writeTimestamp(d.getTime))
  implicit lazy val StringCodec: GenCodec[String] =
    nullableSimple(_.readString(), _ writeString _)
  implicit lazy val SymbolCodec: GenCodec[Symbol] =
    nullableSimple(i => Symbol(i.readString()), (o, s) => o.writeString(s.name))
  implicit lazy val ByteArrayCodec: GenCodec[Array[Byte]] =
    nullableSimple(_.readBinary(), _ writeBinary _)
  implicit lazy val UuidCodec: GenCodec[UUID] =
    nullableSimple(i => UUID.fromString(i.readString()), (o, v) => o.writeString(v.toString))

  implicit lazy val TimestampCodec: GenCodec[Timestamp] =
    GenCodec.nonNullSimple(i => Timestamp(i.readTimestamp()), (o, t) => o.writeTimestamp(t.millis))
  implicit lazy val BytesCodec: GenCodec[Bytes] =
    GenCodec.nullableSimple(i => Bytes(i.readBinary()), (o, b) => o.writeBinary(b.bytes))

  private def declareSize(o: SequentialOutput, coll: BIterable[_]): Unit =
    o.sizePolicy match {
      case SizePolicy.Ignored =>
      case SizePolicy.Optional =>
        coll.knownSize match {
          case -1 =>
          case size => o.declareSize(size)
        }
      case SizePolicy.Required =>
        o.declareSize(coll.size)
    }

  private implicit class IterableOps[A](private val coll: BIterable[A]) extends AnyVal {
    def writeToList(lo: ListOutput)(implicit writer: GenCodec[A]): Unit = {
      declareSize(lo, coll)
      coll.foreach(new (A => Unit) {
        private var idx = 0
        def apply(a: A): Unit = {
          try writer.write(lo.writeElement(), a) catch {
            case NonFatal(e) => throw ListElementWriteFailed(idx, e)
          }
          idx += 1
        }
      })
    }
  }

  private implicit class PairIterableOps[A, B](private val coll: BIterable[(A, B)]) extends AnyVal {
    def writeToObject(oo: ObjectOutput)(implicit keyWriter: GenKeyCodec[A], writer: GenCodec[B]): Unit = {
      declareSize(oo, coll)
      coll.foreach { case (key, value) =>
        val fieldName = keyWriter.write(key)
        try writer.write(oo.writeField(fieldName), value) catch {
          case NonFatal(e) => throw MapFieldWriteFailed(fieldName, e)
        }
      }
    }
  }

  private implicit class ListInputOps(private val li: ListInput) extends AnyVal {
    def collectTo[A: GenCodec, C](implicit fac: Factory[A, C]): C = {
      val b = fac.newBuilder
      li.knownSize match {
        case -1 =>
        case size => b.sizeHint(size)
      }
      var idx = 0
      while (li.hasNext) {
        val a = try read[A](li.nextElement()) catch {
          case NonFatal(e) => throw ListElementReadFailed(idx, e)
        }
        b += a
        idx += 1
      }
      b.result()
    }
  }

  private implicit class ObjectInputOps(private val oi: ObjectInput) extends AnyVal {
    def collectTo[K: GenKeyCodec, V: GenCodec, C](implicit fac: Factory[(K, V), C]): C = {
      val b = fac.newBuilder
      oi.knownSize match {
        case -1 =>
        case size => b.sizeHint(size)
      }
      while (oi.hasNext) {
        val fi = oi.nextField()
        val entry = try ((GenKeyCodec.read[K](fi.fieldName), read[V](fi))) catch {
          case NonFatal(e) => throw MapFieldReadFailed(fi.fieldName, e)
        }
        b += entry
      }
      b.result()
    }
  }

  implicit def arrayCodec[T: ClassTag : GenCodec]: GenCodec[Array[T]] =
    nullableList[Array[T]](_.iterator(read[T]).toArray[T], (lo, arr) => {
      lo.declareSize(arr.length)
      @tailrec def loop(idx: Int): Unit =
        if (idx < arr.length) {
          GenCodec.write(lo.writeElement(), arr(idx))
          loop(idx + 1)
        }
      loop(0)
    })

  // these are covered by the generic `seqCodec` and `setCodec` but making them explicit may be easier
  // for the compiler and also make IntelliJ less confused
  // https://github.com/scala/bug/issues/11027 - only for Scala 2.12
  implicit def bseqCodec[T: GenCodec]: GenCodec[BSeq[T]] = seqCodec[BSeq, T](GenCodec[T], implicitly[Factory[T, List[T]]])
  implicit def iseqCodec[T: GenCodec]: GenCodec[ISeq[T]] = seqCodec[ISeq, T](GenCodec[T], implicitly[Factory[T, List[T]]])
  implicit def mseqCodec[T: GenCodec]: GenCodec[MSeq[T]] = seqCodec[MSeq, T]
  implicit def bindexedSeqCodec[T: GenCodec]: GenCodec[BIndexedSeq[T]] = seqCodec[BIndexedSeq, T]
  implicit def iindexedSeqCodec[T: GenCodec]: GenCodec[IIndexedSeq[T]] = seqCodec[IIndexedSeq, T]
  implicit def mindexedSeqCodec[T: GenCodec]: GenCodec[MIndexedSeq[T]] = seqCodec[MIndexedSeq, T]
  implicit def listCodec[T: GenCodec]: GenCodec[List[T]] = seqCodec[List, T]
  implicit def vectorCodec[T: GenCodec]: GenCodec[Vector[T]] = seqCodec[Vector, T]
  implicit def bsetCodec[T: GenCodec]: GenCodec[BSet[T]] = setCodec[BSet, T]
  implicit def isetCodec[T: GenCodec]: GenCodec[ISet[T]] = setCodec[ISet, T]
  implicit def msetCodec[T: GenCodec]: GenCodec[MSet[T]] = setCodec[MSet, T]
  implicit def ihashSetCodec[T: GenCodec]: GenCodec[IHashSet[T]] = setCodec[IHashSet, T]
  implicit def mhashSetCodec[T: GenCodec]: GenCodec[MHashSet[T]] = setCodec[MHashSet, T]

  // seqCodec, setCodec, jCollectionCodec, mapCodec, jMapCodec, fallbackMapCodec and fallbackJMapCodec
  // have these weird return types (e.g. GenCodec[C[T] with BSeq[T]] instead of just GenCodec[C[T]]) because it's a
  // workaround for https://groups.google.com/forum/#!topic/scala-user/O_fkaChTtg4

  implicit def seqCodec[C[X] <: BSeq[X], T: GenCodec](
    implicit fac: Factory[T, C[T]]
  ): GenCodec[C[T] with BSeq[T]] =
    nullableList[C[T] with BSeq[T]](_.collectTo[T, C[T]], (lo, c) => c.writeToList(lo))

  implicit def setCodec[C[X] <: BSet[X], T: GenCodec](
    implicit fac: Factory[T, C[T]]
  ): GenCodec[C[T] with BSet[T]] =
    nullableList[C[T] with BSet[T]](_.collectTo[T, C[T]], (lo, c) => c.writeToList(lo))

  implicit def jCollectionCodec[C[X] <: JCollection[X], T: GenCodec](
    implicit cbf: JFactory[T, C[T]]
  ): GenCodec[C[T] with JCollection[T]] =
    nullableList[C[T]](_.collectTo[T, C[T]], (lo, c) => c.asScala.writeToList(lo))

  implicit def mapCodec[M[X, Y] <: BMap[X, Y], K: GenKeyCodec, V: GenCodec](
    implicit fac: Factory[(K, V), M[K, V]]
  ): GenObjectCodec[M[K, V]] =
    nullableObject[M[K, V]](
      _.collectTo[K, V, M[K, V]],
      (oo, value) => value.writeToObject(oo)
    )

  implicit def jMapCodec[M[X, Y] <: JMap[X, Y], K: GenKeyCodec, V: GenCodec](
    implicit cbf: JFactory[(K, V), M[K, V]]
  ): GenObjectCodec[M[K, V]] =
    nullableObject[M[K, V]](
      _.collectTo[K, V, M[K, V]],
      (oo, value) => value.asScala.writeToObject(oo)
    )

  implicit def optionCodec[T: GenCodec]: GenCodec[Option[T]] = create[Option[T]](
    input =>
      if (input.legacyOptionEncoding) {
        val li = input.readList()
        val res = if (li.hasNext) Some(read[T](li.nextElement())) else None
        li.skipRemaining()
        res
      }
      else if (input.readNull()) None
      else Some(read[T](input)),

    (output, valueOption) =>
      if (output.legacyOptionEncoding) {
        val lo = output.writeList()
        valueOption.foreach(v => write[T](lo.writeElement(), v))
        lo.finish()
      } else valueOption match {
        case Some(v) => write[T](output, v)
        case None => output.writeNull()
      }
  )

  implicit def nOptCodec[T: GenCodec]: GenCodec[NOpt[T]] =
    new Transformed[NOpt[T], Option[T]](optionCodec[T], _.toOption, _.toNOpt)

  implicit def optCodec[T: GenCodec]: GenCodec[Opt[T]] =
    create[Opt[T]](
      i => if (i.readNull()) Opt.Empty else Opt(read[T](i)),
      (o, vo) => vo match {
        case Opt(v) => write[T](o, v)
        case Opt.Empty => o.writeNull()
      }
    )

  implicit def optArgCodec[T: GenCodec]: GenCodec[OptArg[T]] =
    new Transformed[OptArg[T], Opt[T]](optCodec[T], _.toOpt, _.toOptArg)

  implicit def optRefCodec[T >: Null : GenCodec]: GenCodec[OptRef[T]] =
    new Transformed[OptRef[T], Opt[T]](optCodec[T], _.toOpt, _.toOptRef)

  implicit def eitherCodec[A: GenCodec, B: GenCodec]: GenCodec[Either[A, B]] = nullableObject(
    oi => {
      val fi = oi.nextField()
      fi.fieldName match {
        case "Left" => Left(read[A](fi))
        case "Right" => Right(read[B](fi))
        case name => throw new ReadFailure(s"Expected field 'Left' or 'Right', got $name")
      }
    },
    (oo, v) => v match {
      case Left(a) => write[A](oo.writeField("Left"), a)
      case Right(b) => write[B](oo.writeField("Right"), b)
    }
  )

  implicit def jEnumCodec[E <: Enum[E] : ClassTag]: GenCodec[E] = nullableSimple(
    in => Enum.valueOf(classTag[E].runtimeClass.asInstanceOf[Class[E]], in.readString()),
    (out, value) => out.writeString(value.name)
  )

  // Warning! Changing the order of implicit params of this method causes divergent implicit expansion (WTF?)
  implicit def fromTransparentWrapping[R, T](implicit
    tw: TransparentWrapping[R, T], wrappedCodec: GenCodec[R]
  ): GenCodec[T] =
    new Transformed(wrappedCodec, tw.unwrap, tw.wrap)

  implicit def fromFallback[T](implicit fallback: Fallback[GenCodec[T]]): GenCodec[T] =
    fallback.value
}

trait RecursiveAutoCodecs { this: GenCodec.type =>
  /**
    * Like `materialize`, but descends into types that `T` is made of (e.g. case class field types).
    */
  def materializeRecursively[T]: GenCodec[T] =
  macro macros.serialization.GenCodecMacros.materializeRecursively[T]

  /**
    * INTERNAL API. Should not be used directly.
    */
  implicit def materializeImplicitly[T](implicit allow: AllowImplicitMacro[GenCodec[T]]): GenCodec[T] =
  macro macros.serialization.GenCodecMacros.materializeImplicitly[T]
}
