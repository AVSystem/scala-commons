package com.avsystem.commons
package serialization

import com.avsystem.commons.collection.CollectionAliases._
import com.avsystem.commons.derivation.{AutoDeriveRecursively, DeferredInstance}
import com.avsystem.commons.jiop.BasicJavaInterop._
import com.avsystem.commons.jiop.JCanBuildFrom
import com.avsystem.commons.misc.{NOpt, Opt, OptRef}

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

  case class Auto[T](codec: GenCodec[T]) extends AnyVal

  def read[T](input: Input)(implicit codec: GenCodec[T]): T =
    codec.read(input)

  def write[T](output: Output, value: T)(implicit codec: GenCodec[T]): Unit =
    codec.write(output, value)

  def autoRead[T](input: Input)(implicit autoCodec: GenCodec.Auto[T]): T =
    autoCodec.codec.read(input)

  def autoWrite[T](output: Output, value: T)(implicit autoCodec: GenCodec.Auto[T]): Unit =
    autoCodec.codec.write(output, value)

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
      li.skipRemaining()
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
      oi.skipRemaining()
      result
    }
  }

  trait ErrorReportingCodec[T] extends GenCodec[T] {
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

  implicit def nOptCodec[T: GenCodec]: GenCodec[NOpt[T]] =
    new TransformedCodec[NOpt[T], Option[T]](optionCodec[T], _.toOption, _.toNOpt)

  implicit def optCodec[T: GenCodec]: GenCodec[Opt[T]] =
    create[Opt[T]](
      i => i.readNull().map(_ => Opt.Empty).getOrElse(_ => Opt(read[T](i))),
      locally {
        case (o, Opt(t)) => write[T](o, t)
        case (o, Opt.Empty) => o.writeNull()
      }
    )

  implicit def optRefCodec[T >: Null : GenCodec]: GenCodec[OptRef[T]] =
    new TransformedCodec[OptRef[T], Opt[T]](optCodec[T], _.toOpt, opt => OptRef(opt.orNull))
}

/**
  * Contains readers for maps where there is no DBKeyCodec for key type. In such case, we assume reading from a list
  * of key-value pairs instead of JSON object.
  */
trait FallbackMapCodecs extends RecursiveAutoCodecs {this: GenCodec.type =>
  private def readKVPair[K: GenCodec, V: GenCodec](input: ObjectInput): (K, V) = {
    var keyOpt: NOpt[K] = NOpt.empty
    var valueOpt: NOpt[V] = NOpt.empty
    while (input.hasNext) {
      input.nextField() match {
        case ("k", i) => keyOpt = NOpt.some(read[K](i))
        case ("v", i) => valueOpt = NOpt.some(read[V](i))
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

trait RecursiveAutoCodecs {this: GenCodec.type =>
  def recursiveAuto[T]: GenCodec[T] =
  macro macros.serialization.GenCodecMacros.autoDeriveRecursively[T]

  implicit def implicitRecursiveAuto[T](implicit allow: AutoDeriveRecursively[GenCodec]): GenCodec[T] =
  macro macros.serialization.GenCodecMacros.autoDeriveRecursivelyImplicitly[T]

  implicit def wrappedRecursiveAuto[T]: GenCodec.Auto[T] =
  macro macros.serialization.GenCodecMacros.autoDeriveWrapped[T]
}
