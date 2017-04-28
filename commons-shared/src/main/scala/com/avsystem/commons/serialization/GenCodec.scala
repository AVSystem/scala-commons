package com.avsystem.commons
package serialization

import com.avsystem.commons.annotation.explicitGenerics
import com.avsystem.commons.derivation.{AllowImplicitMacro, DeferredInstance}
import com.avsystem.commons.jiop.JCanBuildFrom

import scala.annotation.implicitNotFound
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

/**
  * Type class for types that can be serialized to [[Output]] (format-agnostic "output stream") and deserialized
  * from [[Input]] (format-agnostic "input stream"). `GenCodec` is supposed to capture generic structure of serialized
  * objects, without being bound to particular format like JSON. The actual format is determined by implementation
  * of [[Input]] and [[Output]].
  *
  * There are convenient macros for automatic derivation of [[GenCodec]] instances (`materialize` and `materializeRecursively`).
  * However, [[GenCodec]] instances still need to be explicitly declared and won't be derived "automagically".
  * If you want fully automatic derivation, use [[GenCodec.Auto]].
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
}

object GenCodec extends FallbackMapCodecs with TupleGenCodecs {
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
  def fromApplyUnapplyProvider[T](applyUnapplyProvider: Any): GenCodec[T] = macro macros.serialization.GenCodecMacros.fromApplyUnapplyProvider[T]

  /**
    * Wrapper over `GenCodec` which forces fully automatic derivation.
    * <p/>
    * If you ask for implicit value of type `GenCodec[T]`, the codec must be explicitly declared and imported or
    * put into implicit scope (e.g. companion object of `T`), even though it can be automatically implemented
    * using `materialize` or `materializeRecursively`.
    * <p/>
    * However, if you ask for implicit value of type `GenCodec.Auto[T]`, the compiler will always fall back to fully
    * automatic derivation if it cannot find already declared `GenCodec`. Note that since `GenCodec.Auto` will always
    * try to wrap already existing `GenCodec` (if there is one), you should never explicitly declare any instances
    * of `GenCodec.Auto`. If you need custom serialization, just write a `GenCodec` and `GenCodec.Auto` will wrap it.
    * <p/>
    * Whether you want to use `GenCodec` or `GenCodec.Auto` depends on your use case. `GenCodec` should be generally
    * used when you want the programmer to always explicitly state that some type is serializable. For example, if you
    * serialize your objects in order to put them into database, you probably want to use `GenCodec` and not `GenCodec.Auto`,
    * because every type that has been written to database is likely to be bound by backwards compatibility constraints and
    * cannot be freely refactored. That's why you want to always explicitly make the decision of making a type serializable.
    * <p/>
    */
  case class Auto[T](codec: GenCodec[T]) extends AnyVal

  @explicitGenerics
  def read[T](input: Input)(implicit codec: GenCodec[T]): T =
    codec.read(input)

  def write[T](output: Output, value: T)(implicit codec: GenCodec[T]): Unit =
    codec.write(output, value)

  @explicitGenerics
  def autoRead[T](input: Input)(implicit autoCodec: GenCodec.Auto[T]): T =
    autoCodec.codec.read(input)

  def autoWrite[T](output: Output, value: T)(implicit autoCodec: GenCodec.Auto[T]): Unit =
    autoCodec.codec.write(output, value)

  def create[T](readFun: Input => T, writeFun: (Output, T) => Any): GenCodec[T] =
    new GenCodec[T] {
      def write(output: Output, value: T) = writeFun(output, value)
      def read(input: Input): T = readFun(input)
    }

  def transformed[T, R: GenCodec](toRaw: T => R, fromRaw: R => T): GenCodec[T] =
    new TransformedCodec[T, R](implicitly[GenCodec[R]], toRaw, fromRaw)

  def createNullSafe[T](readFun: Input => T, writeFun: (Output, T) => Any, allowNull: Boolean): GenCodec[T] =
    new NullSafeCodec[T] {
      protected def nullable = allowNull
      protected def readNonNull(input: Input) = readFun(input)
      protected def writeNonNull(output: Output, value: T) = writeFun(output, value)
    }

  def createList[T](readFun: ListInput => T, writeFun: (ListOutput, T) => Any, allowNull: Boolean) =
    new ListCodec[T] {
      protected def nullable = allowNull
      protected def readList(input: ListInput) = readFun(input)
      protected def writeList(output: ListOutput, value: T) = writeFun(output, value)
    }

  def createObject[T](readFun: ObjectInput => T, writeFun: (ObjectOutput, T) => Any, allowNull: Boolean) =
    new ObjectCodec[T] {
      protected def nullable = allowNull
      protected def readObject(input: ObjectInput) = readFun(input)
      protected def writeObject(output: ObjectOutput, value: T) = writeFun(output, value)
    }

  def fromKeyCodec[T](implicit keyCodec: GenKeyCodec[T]): GenCodec[T] = create(
    input => keyCodec.read(input.readString()),
    (output, value) => output.writeString(keyCodec.write(value))
  )

  def forSealedEnum[T]: GenCodec[T] = macro macros.serialization.GenCodecMacros.forSealedEnum[T]

  class ReadFailure(msg: String, cause: Throwable) extends RuntimeException(msg, cause) {
    def this(msg: String) = this(msg, null)

    override def fillInStackTrace(): Throwable =
      if (cause == null) super.fillInStackTrace() else this
  }

  class WriteFailure(msg: String, cause: Throwable) extends RuntimeException(msg, cause) {
    def this(msg: String) = this(msg, null)

    override def fillInStackTrace(): Throwable =
      if (cause == null) super.fillInStackTrace() else this
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
      if (value == null)
        if (nullable) output.writeNull() else throw new WriteFailure("null")
      else writeNonNull(output, value)

    def read(input: Input): T =
      if (input.inputType == InputType.Null)
        if (nullable) input.readNull().asInstanceOf[T] else throw new ReadFailure("null")
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
      val li = input.readList()
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
      val oi = input.readObject()
      val result = readObject(oi)
      oi.skipRemaining()
      result
    }
  }

  trait ErrorReportingCodec[T] extends GenCodec[T] {
    protected def typeRepr: String

    protected def readField[A](fieldInput: FieldInput, codec: GenCodec[A]): A =
      decoratedRead(fieldInput, codec, "field")

    protected def readCase[A](fieldInput: FieldInput, codec: GenCodec[A]): A =
      decoratedRead(fieldInput, codec, "case")

    private def decoratedRead[A](fieldInput: FieldInput, codec: GenCodec[A], what: String): A =
      try codec.read(fieldInput) catch {
        case NonFatal(e) => throw new ReadFailure(s"Failed to read $what ${fieldInput.fieldName} of $typeRepr", e)
      }

    protected def writeField[A](fieldName: String, output: ObjectOutput, value: A, codec: GenCodec[A]): Unit =
      decoratedWrite(fieldName, output, value, codec, "field")

    protected def writeCase[A](fieldName: String, output: ObjectOutput, value: A, codec: GenCodec[A]): Unit =
      decoratedWrite(fieldName, output, value, codec, "case")

    private def decoratedWrite[A](fieldName: String, output: ObjectOutput, value: A, codec: GenCodec[A], what: String): Unit =
      try codec.write(output.writeField(fieldName), value) catch {
        case NonFatal(e) => throw new WriteFailure(s"Failed to write $what $fieldName of $typeRepr", e)
      }

    protected def fieldMissing(field: String) =
      throw new ReadFailure(s"Cannot read $typeRepr, field $field is missing in decoded data")

    protected def unknownCase(field: String) =
      throw new ReadFailure(s"Cannot read $typeRepr, unknown case: $field")

    protected def notSingleField(empty: Boolean) =
      throw new ReadFailure(s"Cannot read $typeRepr, expected object with exactly one field but got ${if (empty) "empty object" else "more than one"}")

    protected def unapplyFailed =
      throw new WriteFailure(s"Could not write $typeRepr, unapply/unapplySeq returned false or empty value")
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
  implicit val NullCodec: GenCodec[Null] = create[Null](_.readNull(), (o, _) => o.writeNull())
  implicit val UnitCodec: GenCodec[Unit] = create[Unit](_.readUnit(), (o, _) => o.writeUnit())
  implicit val VoidCodec: GenCodec[Void] = create[Void](_.readNull(), (o, _) => o.writeNull())

  implicit val BooleanCodec: GenCodec[Boolean] = create(_.readBoolean(), _ writeBoolean _)
  implicit val CharCodec: GenCodec[Char] = create(_.readChar(), _ writeChar _)
  implicit val ByteCodec: GenCodec[Byte] = create(_.readByte(), _ writeByte _)
  implicit val ShortCodec: GenCodec[Short] = create(_.readShort(), _ writeShort _)
  implicit val IntCodec: GenCodec[Int] = create(_.readInt(), _ writeInt _)
  implicit val LongCodec: GenCodec[Long] = create(_.readLong(), _ writeLong _)
  implicit val FloatCodec: GenCodec[Float] = create(_.readFloat(), _ writeFloat _)
  implicit val DoubleCodec: GenCodec[Double] = create(_.readDouble(), _ writeDouble _)

  implicit val JBooleanCodec: GenCodec[JBoolean] = createNullSafe(_.readBoolean(), _ writeBoolean _, allowNull = true)
  implicit val JCharacterCodec: GenCodec[JCharacter] = createNullSafe(_.readChar(), _ writeChar _, allowNull = true)
  implicit val JByteCodec: GenCodec[JByte] = createNullSafe(_.readByte(), _ writeByte _, allowNull = true)
  implicit val JShortCodec: GenCodec[JShort] = createNullSafe(_.readShort(), _ writeShort _, allowNull = true)
  implicit val JIntegerCodec: GenCodec[JInteger] = createNullSafe(_.readInt(), _ writeInt _, allowNull = true)
  implicit val JLongCodec: GenCodec[JLong] = createNullSafe(_.readLong(), _ writeLong _, allowNull = true)
  implicit val JFloatCodec: GenCodec[JFloat] = createNullSafe(_.readFloat(), _ writeFloat _, allowNull = true)
  implicit val JDoubleCodec: GenCodec[JDouble] = createNullSafe(_.readDouble(), _ writeDouble _, allowNull = true)

  implicit val JDateCodec: GenCodec[JDate] = createNullSafe(i => new JDate(i.readTimestamp()), (o, d) => o.writeTimestamp(d.getTime), allowNull = true)
  implicit val StringCodec: GenCodec[String] = createNullSafe(_.readString(), _ writeString _, allowNull = true)
  implicit val ByteArrayCodec: GenCodec[Array[Byte]] = createNullSafe(_.readBinary(), _ writeBinary _, allowNull = true)

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
    createList[Array[T]](_.iterator(read[T]).toArray[T], (lo, arr) => arr.iterator.writeToList(lo), allowNull = true)

  // seqCodec, setCodec, jCollectionCodec, mapCodec, jMapCodec, fallbackMapCodec and fallbackJMapCodec
  // have these weird return types (e.g. GenCodec[C[T] with BSeq[T]] instead of just GenCodec[C[T]]) because it's a
  // workaround for https://groups.google.com/forum/#!topic/scala-user/O_fkaChTtg4

  implicit def seqCodec[C[X] <: BSeq[X], T: GenCodec](
    implicit cbf: CanBuildFrom[Nothing, T, C[T]]): GenCodec[C[T] with BSeq[T]] =
    createList[C[T] with BSeq[T]](readListWithCBF[C, T], (lo, c) => c.iterator.writeToList(lo), allowNull = true)

  implicit def setCodec[C[X] <: BSet[X], T: GenCodec](
    implicit cbf: CanBuildFrom[Nothing, T, C[T]]): GenCodec[C[T] with BSet[T]] =
    createList[C[T] with BSet[T]](readListWithCBF[C, T], (lo, c) => c.iterator.writeToList(lo), allowNull = true)

  implicit def jCollectionCodec[C[X] <: JCollection[X], T: GenCodec](
    implicit cbf: JCanBuildFrom[T, C[T]]): GenCodec[C[T] with JCollection[T]] =
    createList[C[T] with JCollection[T]](readListWithCBF[C, T], (lo, c) => c.iterator.asScala.writeToList(lo), allowNull = true)

  implicit def mapCodec[M[X, Y] <: BMap[X, Y], K: GenKeyCodec, V: GenCodec](
    implicit cbf: CanBuildFrom[Nothing, (K, V), M[K, V]]): GenCodec[M[K, V] with BMap[K, V]] =
    createObject[M[K, V] with BMap[K, V]](
      _.iterator(read[V]).map({ case (k, v) => (GenKeyCodec.read[K](k), v) }).collectWith(cbf),
      (oo, value) => value.foreach({ case (k, v) => write[V](oo.writeField(GenKeyCodec.write(k)), v) }),
      allowNull = true
    )

  implicit def jMapCodec[M[X, Y] <: JMap[X, Y], K: GenKeyCodec, V: GenCodec](
    implicit cbf: JCanBuildFrom[(K, V), M[K, V]]): GenCodec[M[K, V] with JMap[K, V]] =
    createObject[M[K, V] with JMap[K, V]](
      _.iterator(read[V]).map({ case (k, v) => (GenKeyCodec.read[K](k), v) }).collectWith(cbf),
      (oo, value) => value.asScala.foreach({ case (k, v) => write[V](oo.writeField(GenKeyCodec.write(k)), v) }),
      allowNull = true
    )

  implicit def optionCodec[T: GenCodec]: GenCodec[Option[T]] =
    createList[Option[T]](
      li => if (li.hasNext) Some(read[T](li.nextElement())) else None,
      (lo, opt) => opt.iterator.writeToList(lo),
      allowNull = true
    )

  implicit def nOptCodec[T: GenCodec]: GenCodec[NOpt[T]] =
    new TransformedCodec[NOpt[T], Option[T]](optionCodec[T], _.toOption, _.toNOpt)

  implicit def optCodec[T: GenCodec]: GenCodec[Opt[T]] =
    create[Opt[T]](
      i => i.inputType match {
        case InputType.Null =>
          i.readNull()
          Opt.Empty
        case _ =>
          Opt(read[T](i))
      },
      locally {
        case (o, Opt(t)) => write[T](o, t)
        case (o, Opt.Empty) => o.writeNull()
      }
    )

  implicit def optArgCodec[T: GenCodec]: GenCodec[OptArg[T]] =
    new TransformedCodec[OptArg[T], Opt[T]](optCodec[T], _.toOpt, opt => if (opt.isEmpty) OptArg.Empty else OptArg(opt.get))

  implicit def optRefCodec[T >: Null : GenCodec]: GenCodec[OptRef[T]] =
    new TransformedCodec[OptRef[T], Opt[T]](optCodec[T], _.toOpt, opt => OptRef(opt.orNull))

  implicit def jEnumCodec[E <: Enum[E] : ClassTag]: GenCodec[E] = createNullSafe(
    in => Enum.valueOf(classTag[E].runtimeClass.asInstanceOf[Class[E]], in.readString()),
    (out, value) => out.writeString(value.name),
    allowNull = true
  )

  // Needed because of SI-9453
  implicit val NothingAutoCodec: GenCodec.Auto[Nothing] = GenCodec.Auto[Nothing](NothingCodec)
}

/**
  * Contains readers for maps where there is no DBKeyCodec for key type. In such case, we assume reading from a list
  * of key-value pairs instead of JSON object.
  */
trait FallbackMapCodecs extends RecursiveAutoCodecs { this: GenCodec.type =>
  private def readKVPair[K: GenCodec, V: GenCodec](input: ObjectInput): (K, V) = {
    val key = read[K](input.nextField().assertField("k"))
    val value = read[V](input.nextField().assertField("v"))
    (key, value)
  }

  private def writeKVPair[K, V](output: ObjectOutput, key: K, value: V)(implicit keyCodec: GenCodec[K], valueCodec: GenCodec[V]): Unit = {
    keyCodec.write(output.writeField("k"), key)
    valueCodec.write(output.writeField("v"), value)
    output.finish()
  }

  implicit def fallbackMapCodec[M[X, Y] <: BMap[X, Y], K: GenCodec, V: GenCodec](
    implicit cbf: CanBuildFrom[Nothing, (K, V), M[K, V]]): GenCodec[M[K, V] with BMap[K, V]] =
    createList[M[K, V] with BMap[K, V]](
      _.iterator(i => readKVPair[K, V](i.readObject())).collectWith(cbf),
      (lo, map) => map.iterator.foreach({ case (k, v) => writeKVPair(lo.writeElement().writeObject(), k, v) }),
      allowNull = true
    )

  implicit def fallbackJMapCodec[M[X, Y] <: JMap[X, Y], K: GenCodec, V: GenCodec](
    implicit cbf: JCanBuildFrom[(K, V), M[K, V]]): GenCodec[M[K, V] with JMap[K, V]] =
    createList[M[K, V] with JMap[K, V]](
      _.iterator(i => readKVPair[K, V](i.readObject())).collectWith(cbf),
      (lo, map) => map.asScala.iterator.foreach({ case (k, v) => writeKVPair(lo.writeElement().writeObject(), k, v) }),
      allowNull = true
    )
}

trait RecursiveAutoCodecs { this: GenCodec.type =>
  /**
    * Like `materialize`, but descends into types that `T` is made of (e.g. case class field types).
    */
  def materializeRecursively[T]: GenCodec[T] =
  macro macros.serialization.GenCodecMacros.materializeRecursively[T]

  /**
    * Used internally for materialization of `GenCodec.Auto`. Should not be used directly.
    */
  implicit def materializeImplicitly[T](implicit allow: AllowImplicitMacro[GenCodec[T]]): GenCodec[T] =
  macro macros.serialization.GenCodecMacros.materializeImplicitly[T]

  implicit def materializeAuto[T]: GenCodec.Auto[T] =
  macro macros.serialization.GenCodecMacros.materializeAuto[T]
}
