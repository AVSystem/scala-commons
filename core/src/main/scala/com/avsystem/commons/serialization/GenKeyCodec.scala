package com.avsystem.commons
package serialization

import java.util.UUID

import com.avsystem.commons.annotation.explicitGenerics
import com.avsystem.commons.misc.{Bytes, Timestamp}
import com.avsystem.commons.serialization.GenCodec.{ReadFailure, WriteFailure}

import scala.annotation.implicitNotFound

/** Typeclass which implements two-directional conversion between values of some type and field names used in
  * [[ObjectOutput.writeField]] and [[ObjectInput.nextField]] ([[FieldInput.fieldName]]). Every type which has a
  * natural, unambiguous string representation should have a `GenKeyCodec`.
  */
@implicitNotFound("No GenKeyCodec found for ${T}")
trait GenKeyCodec[T] {
  def read(key: String): T
  def write(value: T): String

  final def transform[U](onWrite: U => T, onRead: T => U): GenKeyCodec[U] =
    new GenKeyCodec.Transformed(this, onWrite, onRead)
}

object GenKeyCodec {
  def apply[T](implicit gkc: GenKeyCodec[T]): GenKeyCodec[T] = gkc

  // TODO[scala3-port]: GenKeyCodec.forSealedEnum (Scala 2 macro def) (L)
  def forSealedEnum[T]: GenKeyCodec[T] = ???

  // TODO[scala3-port]: GenKeyCodec.forTransparentWrapper (Scala 2 macro def) (L)
  def forTransparentWrapper[T]: GenKeyCodec[T] = ???

  @explicitGenerics
  def read[T](key: String)(implicit keyCodec: GenKeyCodec[T]): T = keyCodec.read(key)
  def write[T](value: T)(implicit keyCodec: GenKeyCodec[T]): String = keyCodec.write(value)

  def create[T](readFun: String => T, writeFun: T => String): GenKeyCodec[T] =
    new GenKeyCodec[T] {
      def read(key: String): T = readFun(key)
      def write(value: T): String = writeFun(value)
    }

  final class Transformed[A, B](val wrapped: GenKeyCodec[B], onWrite: A => B, onRead: B => A) extends GenKeyCodec[A] {
    def read(key: String): A = {
      val wrappedValue = wrapped.read(key)
      try onRead(wrappedValue)
      catch {
        case NonFatal(cause) => throw new ReadFailure(s"onRead conversion failed", cause)
      }
    }

    def write(value: A): String = {
      val wrappedValue =
        try onWrite(value)
        catch {
          case NonFatal(cause) => throw new WriteFailure(s"onWrite conversion failed", cause)
        }
      wrapped.write(wrappedValue)
    }
  }

  given BooleanKeyCodec: GenKeyCodec[Boolean] = create(_.toBoolean, _.toString)
  given CharKeyCodec: GenKeyCodec[Char] = create(_.charAt(0), _.toString)
  given ByteKeyCodec: GenKeyCodec[Byte] = create(_.toByte, _.toString)
  given ShortKeyCodec: GenKeyCodec[Short] = create(_.toShort, _.toString)
  given IntKeyCodec: GenKeyCodec[Int] = create(_.toInt, _.toString)
  given LongKeyCodec: GenKeyCodec[Long] = create(_.toLong, _.toString)
  given BigIntKeyCodec: GenKeyCodec[BigInt] = create(BigInt(_), _.toString)

  given JBooleanKeyCodec: GenKeyCodec[JBoolean] = create(_.toBoolean, _.toString)
  given JCharacterKeyCodec: GenKeyCodec[JCharacter] = create(_.charAt(0), _.toString)
  given JByteKeyCodec: GenKeyCodec[JByte] = create(_.toByte, _.toString)
  given JShortKeyCodec: GenKeyCodec[JShort] = create(_.toShort, _.toString)
  given JIntKeyCodec: GenKeyCodec[JInteger] = create(_.toInt, _.toString)
  given JLongKeyCodec: GenKeyCodec[JLong] = create(_.toLong, _.toString)
  given JBigIntegerKeyCodec: GenKeyCodec[JBigInteger] = create(new JBigInteger(_), _.toString)

  given StringKeyCodec: GenKeyCodec[String] = create(identity, identity)
  given SymbolKeyCodec: GenKeyCodec[Symbol] = create(Symbol(_), _.name)
  given UUIDKeyCodec: GenKeyCodec[UUID] = create(UUID.fromString, _.toString)

  given TimestampKeyCodec: GenKeyCodec[Timestamp] = GenKeyCodec.create(Timestamp.parse, _.toString)
  given BytesKeyCodec: GenKeyCodec[Bytes] = GenKeyCodec.create(Bytes.fromBase64(_), _.base64)

  given jEnumKeyCodec: [E <: Enum[E]] => (ct: ClassTag[E]) => GenKeyCodec[E] =
    GenKeyCodec.create(
      string => Enum.valueOf(ct.runtimeClass.asInstanceOf[Class[E]], string),
      e => e.name(),
    )

  // Warning! Changing the order of implicit params of this method causes divergent implicit expansion (WTF?)
  given fromTransparentWrapping
    : [R, T] => (tw: TransparentWrapping[R, T]) => (wrappedCodec: GenKeyCodec[R]) => GenKeyCodec[T] =
    new Transformed(wrappedCodec, tw.unwrap, tw.wrap)

}
