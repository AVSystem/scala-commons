package com.avsystem.commons
package serialization

import com.avsystem.commons.collection.CollectionAliases._
import com.avsystem.commons.jiop.BasicJavaInterop._
import com.avsystem.commons.jiop.JCanBuildFrom
import com.avsystem.commons.macros.DeferredInstance

import scala.annotation.implicitNotFound
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.reflect.ClassTag

@implicitNotFound("No GenCodec found for ${T}")
trait GenCodec[T] {
  def read(input: Input): T
  def write(output: Output, value: T): Unit
}

object GenCodec extends FallbackMapCodecs with TupleGenCodecs {
  /**
    * Macro that automatically materializes a [[GenCodec]] for some type [[T]], which must be one of:
    * <ul>
    * <li>singleton type, e.g. an `object`</li>
    * <li>case class whose every field type has its own [[GenCodec]]</li>
    * <li>(generalization of case classes) class or trait whose companion object has a pair of case-class-like `apply`
    * and `unapply` methods and every parameter type of `apply` method has its own [[GenCodec]]
    * </li>
    * <li>sealed hierarchy in which every non-abstract subclass either has its own [[GenCodec]] or it can be
    * automatically materialized with the same mechanism</li>
    * </ul>
    */
  def auto[T]: GenCodec[T] = macro macros.serialization.GenCodecMacros.autoDerive[T]

  def read[T](input: Input)(implicit reader: GenCodec[T]): T =
    reader.read(input)

  def write[T](output: Output, value: T)(implicit writer: GenCodec[T]): Any =
    writer.write(output, value)

  def create[T](readFun: Input => T, writeFun: (Output, T) => Any): GenCodec[T] =
    new GenCodec[T] {
      def write(output: Output, value: T) = writeFun(output, value)
      def read(input: Input): T = readFun(input)
    }

  def createNullSafe[T >: Null](readFun: Input => T, writeFun: (Output, T) => Any): GenCodec[T] =
    new NullSafeCodec[T] {
      protected def nullable = true
      protected def readNonNull(input: Input) = readFun(input)
      protected def writeNonNull(output: Output, value: T) = writeFun(output, value)
    }

  def createList[T >: Null](readFun: ListInput => T, writeFun: (ListOutput, T) => Any) =
    new ListCodec[T] {
      protected def nullable = true
      protected def readList(input: ListInput) = readFun(input)
      protected def writeList(output: ListOutput, value: T) = writeFun(output, value)
    }

  def createObject[T >: Null](readFun: ObjectInput => T, writeFun: (ObjectOutput, T) => Any) =
    new ObjectCodec[T] {
      protected def nullable = true
      protected def readObject(input: ObjectInput) = readFun(input)
      protected def writeObject(output: ObjectOutput, value: T) = writeFun(output, value)
    }

  def fromKeyCodec[T](implicit keyCodec: GenKeyCodec[T]): GenCodec[T] = create(
    input => keyCodec.read(input.readString().get),
    (output, value) => output.writeString(keyCodec.write(value))
  )

  class ReadFailure(msg: String, cause: Throwable) extends RuntimeException(msg, cause) {
    def this(msg: String) = this(msg, null)
  }

  class WriteFailure(msg: String, cause: Throwable) extends RuntimeException(msg, cause) {
    def this(msg: String) = this(msg, null)
  }

  final class Deferred[T] extends DeferredInstance[GenCodec[T]] with GenCodec[T] {
    def read(input: Input) = underlying.read(input)
    def write(output: Output, value: T) = underlying.write(output, value)
  }

  trait NullSafeCodec[T] extends GenCodec[T] {
    protected def nullable: Boolean
    protected def readNonNull(input: Input): T
    protected def writeNonNull(output: Output, value: T): Unit

    def write(output: Output, value: T): Unit =
      if (value == null) output.writeNull() else writeNonNull(output, value)

    def read(input: Input): T =
      if (nullable) input.readNull().map(_.asInstanceOf[T]).getOrElse(_ => readNonNull(input))
      else readNonNull(input)
  }

  trait ListCodec[T] extends NullSafeCodec[T] {
    protected def readList(input: ListInput): T
    protected def writeList(output: ListOutput, value: T): Unit

    protected def writeNonNull(output: Output, value: T) = {
      val lo = output.writeList()
      writeList(lo, value)
      lo.finish()
    }
    protected def readNonNull(input: Input) = {
      val li = input.readList().get
      val result = readList(li)
      li.skipAll()
      result
    }
  }

  trait ObjectCodec[T] extends NullSafeCodec[T] {
    protected def readObject(input: ObjectInput): T
    protected def writeObject(output: ObjectOutput, value: T): Unit

    protected def writeNonNull(output: Output, value: T) = {
      val oo = output.writeObject()
      writeObject(oo, value)
      oo.finish()
    }
    protected def readNonNull(input: Input) = {
      val oi = input.readObject().get
      val result = readObject(oi)
      oi.skipAll()
      result
    }
  }

  trait RichObjectCodec[T] extends ObjectCodec[T] {
    protected def typeRepr: String

    protected def fieldMissing(field: String) =
      throw new ReadFailure(s"Cannot read $typeRepr, field $field is missing in decoded data")

    protected def unknownCase(field: String) =
      throw new ReadFailure(s"Cannot read $typeRepr, unknown case: $field")

    protected def notSingleField(empty: Boolean) =
      throw new ReadFailure(s"Cannot read $typeRepr, expected object with exactly one field but got ${if (empty) "empty object" else "more than one"}")

    protected def unapplyFailed =
      throw new WriteFailure(s"Could not write $typeRepr, unapply failed (returned false or None)")
  }

  final class SingletonCodec[T <: Singleton](value: => T) extends ObjectCodec[T] {
    protected def nullable: Boolean = true
    protected def readObject(input: ObjectInput) = value
    protected def writeObject(output: ObjectOutput, value: T) = ()
  }

  class TransformedCodec[A, B](val wrapped: GenCodec[B], onWrite: A => B, onRead: B => A) extends GenCodec[A] {
    def read(input: Input) = onRead(wrapped.read(input))
    def write(output: Output, value: A) = wrapped.write(output, onWrite(value))
  }

  def underlyingCodec(codec: GenCodec[_]): GenCodec[_] = codec match {
    case tc: TransformedCodec[_, _] => underlyingCodec(tc.wrapped)
    case _ => codec
  }

  implicit val NothingCodec: GenCodec[Nothing] = create[Nothing](_ => sys.error("read Nothing"), (_, _) => sys.error("write Nothing"))
  implicit val NullCodec: GenCodec[Null] = create[Null](_.readNull().get, (o, _) => o.writeNull())
  implicit val UnitCodec: GenCodec[Unit] = create[Unit](_.readUnit().get, (o, _) => o.writeUnit())
  implicit val VoidCodec: GenCodec[Void] = create[Void](_.readNull().get, (o, _) => o.writeNull())

  implicit val BooleanCodec: GenCodec[Boolean] = create(_.readBoolean().get, _ writeBoolean _)
  implicit val CharCodec: GenCodec[Char] = create(_.readChar().get, _ writeChar _)
  implicit val ByteCodec: GenCodec[Byte] = create(_.readByte().get, _ writeByte _)
  implicit val ShortCodec: GenCodec[Short] = create(_.readShort().get, _ writeShort _)
  implicit val IntCodec: GenCodec[Int] = create(_.readInt().get, _ writeInt _)
  implicit val LongCodec: GenCodec[Long] = create(_.readLong().get, _ writeLong _)
  implicit val FloatCodec: GenCodec[Float] = create(_.readFloat().get, _ writeFloat _)
  implicit val DoubleCodec: GenCodec[Double] = create(_.readDouble().get, _ writeDouble _)

  implicit val JBooleanCodec: GenCodec[JBoolean] = createNullSafe(_.readBoolean().get, _ writeBoolean _)
  implicit val JCharacterCodec: GenCodec[JCharacter] = createNullSafe(_.readChar().get, _ writeChar _)
  implicit val JByteCodec: GenCodec[JByte] = createNullSafe(_.readByte().get, _ writeByte _)
  implicit val JShortCodec: GenCodec[JShort] = createNullSafe(_.readShort().get, _ writeShort _)
  implicit val JIntegerCodec: GenCodec[JInteger] = createNullSafe(_.readInt().get, _ writeInt _)
  implicit val JLongCodec: GenCodec[JLong] = createNullSafe(_.readLong().get, _ writeLong _)
  implicit val JFloatCodec: GenCodec[JFloat] = createNullSafe(_.readFloat().get, _ writeFloat _)
  implicit val JDoubleCodec: GenCodec[JDouble] = createNullSafe(_.readDouble().get, _ writeDouble _)

  implicit val JDateCodec: GenCodec[JDate] = createNullSafe(i => new JDate(i.readTimestamp().get), (o, d) => o.writeTimestamp(d.getTime))
  implicit val StringCodec: GenCodec[String] = createNullSafe(_.readString().get, _ writeString _)
  implicit val ByteArrayCodec: GenCodec[Array[Byte]] = createNullSafe(_.readBinary().get, _ writeBinary _)

  private implicit class IteratorOps[A](private val it: Iterator[A]) extends AnyVal {
    def writeToList(lo: ListOutput)(implicit writer: GenCodec[A]): Unit =
      it.foreach(writer.write(lo.writeElement(), _))
  }

  protected implicit class TraversableOps[A](private val it: TraversableOnce[A]) extends AnyVal {
    def collectWith[C](cbf: CanBuildFrom[Nothing, A, C]): C = {
      val b = cbf()
      b ++= it
      b.result()
    }
  }

  private def readListWithCBF[C[_], T: GenCodec](li: ListInput)(implicit cbf: CanBuildFrom[Nothing, T, C[T]]): C[T] =
    li.iterator(read[T]).collectWith(cbf)

  implicit def arrayCodec[T: ClassTag : GenCodec]: GenCodec[Array[T]] =
    createList[Array[T]](_.iterator(read[T]).toArray[T], (lo, arr) => arr.iterator.writeToList(lo))

  implicit def seqCodec[C[X] >: Null <: BSeq[X], T: GenCodec](implicit cbf: CanBuildFrom[Nothing, T, C[T]]): GenCodec[C[T]] =
    createList[C[T]](readListWithCBF[C, T], (lo, c) => c.iterator.writeToList(lo))

  implicit def setCodec[C[X] >: Null <: BSet[X], T: GenCodec](implicit cbf: CanBuildFrom[Nothing, T, C[T]]): GenCodec[C[T]] =
    createList[C[T]](readListWithCBF[C, T], (lo, c) => c.iterator.writeToList(lo))

  implicit def jCollectionCodec[C[X] >: Null <: JCollection[X], T: GenCodec](implicit cbf: JCanBuildFrom[T, C[T]]): GenCodec[C[T]] =
    createList[C[T]](readListWithCBF[C, T], (lo, c) => c.iterator.asScala.writeToList(lo))

  implicit def mapCodec[M[X, Y] >: Null <: BMap[X, Y], K: GenKeyCodec, V: GenCodec](implicit cbf: CanBuildFrom[Nothing, (K, V), M[K, V]]): GenCodec[M[K, V]] =
    createObject[M[K, V]](
      _.iterator(read[V]).map({ case (k, v) => (GenKeyCodec.read[K](k), v) }).collectWith(cbf),
      (oo, value) => value.foreach({ case (k, v) => write[V](oo.writeField(GenKeyCodec.write(k)), v) })
    )

  implicit def jMapCodec[M[X, Y] >: Null <: JMap[X, Y], K: GenKeyCodec, V: GenCodec](implicit cbf: JCanBuildFrom[(K, V), M[K, V]]): GenCodec[M[K, V]] =
    createObject[M[K, V]](
      _.iterator(read[V]).map({ case (k, v) => (GenKeyCodec.read[K](k), v) }).collectWith(cbf),
      (oo, value) => value.asScala.foreach({ case (k, v) => write[V](oo.writeField(GenKeyCodec.write(k)), v) })
    )

  implicit def optionCodec[T: GenCodec]: GenCodec[Option[T]] =
    createList[Option[T]](
      li => if (li.hasNext) Some(read[T](li.nextElement())) else None,
      (lo, opt) => opt.iterator.writeToList(lo)
    )
}

/**
  * Contains readers for maps where there is no DBKeyCodec for key type. In such case, we assume reading from a list
  * of key-value pairs instead of JSON object.
  */
trait FallbackMapCodecs {this: GenCodec.type =>
  private def readKVPair[K: GenCodec, V: GenCodec](input: ObjectInput): (K, V) = {
    var keyOpt: Option[K] = None
    var valueOpt: Option[V] = None
    while (input.hasNext) {
      input.nextField() match {
        case ("k", i) => keyOpt = Some(read[K](i))
        case ("v", i) => valueOpt = Some(read[V](i))
        case _ =>
      }
    }
    val key = keyOpt.getOrElse(throw new ReadFailure("key `k` absent"))
    val value = valueOpt.getOrElse(throw new ReadFailure("key `v` absent"))
    (key, value)
  }

  private def writeKVPair[K, V](output: ObjectOutput, key: K, value: V)(implicit keyCodec: GenCodec[K], valueCodec: GenCodec[V]): Unit = {
    keyCodec.write(output.writeField("k"), key)
    valueCodec.write(output.writeField("v"), value)
    output.finish()
  }

  implicit def fallbackMapCodec[M[X, Y] >: Null <: BMap[X, Y], K: GenCodec, V: GenCodec](implicit cbf: CanBuildFrom[Nothing, (K, V), M[K, V]]): GenCodec[M[K, V]] =
    createList[M[K, V]](
      _.iterator(i => readKVPair[K, V](i.readObject().get)).collectWith(cbf),
      (lo, map) => map.iterator.foreach({ case (k, v) => writeKVPair(lo.writeElement().writeObject(), k, v) })
    )

  implicit def fallbackJMapCodec[M[X, Y] >: Null <: JMap[X, Y], K: GenCodec, V: GenCodec](implicit cbf: JCanBuildFrom[(K, V), M[K, V]]): GenCodec[M[K, V]] =
    createList[M[K, V]](
      _.iterator(i => readKVPair[K, V](i.readObject().get)).collectWith(cbf),
      (lo, map) => map.asScala.iterator.foreach({ case (k, v) => writeKVPair(lo.writeElement().writeObject(), k, v) })
    )
}

/**
  * Typeclass which expresses ability to convert between MongoDB JSON object keys and values of some type.
  * Every type which has a natural, unambiguous string representation should have a DBKeyCodec.
  */
@implicitNotFound("Can't convert between database key and ${T} - DBKeyCodec not found")
trait GenKeyCodec[T] {
  def read(key: String): T
  def write(value: T): String
}

object GenKeyCodec {
  def read[T](key: String)(implicit keyCodec: GenKeyCodec[T]): T = keyCodec.read(key)
  def write[T](value: T)(implicit keyCodec: GenKeyCodec[T]): String = keyCodec.write(value)

  def create[T](readFun: String => T, writeFun: T => String): GenKeyCodec[T] =
    new GenKeyCodec[T] {
      def read(key: String): T = readFun(key)
      def write(value: T): String = writeFun(value)
    }

  implicit val BooleanKeyCodec: GenKeyCodec[Boolean] = create(_.toBoolean, _.toString)
  implicit val CharKeyCodec: GenKeyCodec[Char] = create(_.charAt(0), _.toString)
  implicit val ByteKeyCodec: GenKeyCodec[Byte] = create(_.toByte, _.toString)
  implicit val ShortKeyCodec: GenKeyCodec[Short] = create(_.toShort, _.toString)
  implicit val IntKeyCodec: GenKeyCodec[Int] = create(_.toInt, _.toString)
  implicit val LongKeyCodec: GenKeyCodec[Long] = create(_.toLong, _.toString)

  implicit val JBooleanKeyCodec: GenKeyCodec[JBoolean] = create(_.toBoolean, _.toString)
  implicit val JCharacterKeyCodec: GenKeyCodec[JCharacter] = create(_.charAt(0), _.toString)
  implicit val JByteKeyCodec: GenKeyCodec[JByte] = create(_.toByte, _.toString)
  implicit val JShortKeyCodec: GenKeyCodec[JShort] = create(_.toShort, _.toString)
  implicit val JIntKeyCodec: GenKeyCodec[JInteger] = create(_.toInt, _.toString)
  implicit val JLongKeyCodec: GenKeyCodec[JLong] = create(_.toLong, _.toString)

  implicit val StringKeyCodec: GenKeyCodec[String] = create(identity, identity)
}
