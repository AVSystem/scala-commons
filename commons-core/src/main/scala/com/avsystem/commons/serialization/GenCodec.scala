package com.avsystem.commons
package serialization

import com.avsystem.commons.annotation.explicitGenerics
import com.avsystem.commons.derivation.{AllowImplicitMacro, DeferredInstance}
import com.avsystem.commons.jiop.JCanBuildFrom
import com.avsystem.commons.misc.MacroGenerated
import com.github.ghik.silencer.silent

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

object GenCodec extends RecursiveAutoCodecs with TupleGenCodecs {
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
  @deprecated("auto codecs will be removed", "1.28.0")
  case class Auto[T](codec: GenCodec[T]) extends AnyVal

  @explicitGenerics
  def read[T](input: Input)(implicit codec: GenCodec[T]): T =
    codec.read(input)

  def write[T](output: Output, value: T)(implicit codec: GenCodec[T]): Unit =
    codec.write(output, value)

  @explicitGenerics
  @deprecated("auto codecs will be removed", "1.28.0")
  def autoRead[T](input: Input)(implicit autoCodec: GenCodec.Auto[T]): T =
    autoCodec.codec.read(input)

  @deprecated("auto codecs will be removed", "1.28.0")
  def autoWrite[T](output: Output, value: T)(implicit autoCodec: GenCodec.Auto[T]): Unit =
    autoCodec.codec.write(output, value)

  def create[T](readFun: Input => T, writeFun: (Output, T) => Any): GenCodec[T] =
    new GenCodec[T] {
      def write(output: Output, value: T): Unit = writeFun(output, value)
      def read(input: Input) = readFun(input)
    }

  def transformed[T, R: GenCodec](toRaw: T => R, fromRaw: R => T): GenCodec[T] =
    new TransformedCodec[T, R](implicitly[GenCodec[R]], toRaw, fromRaw)

  def createNullSafe[T](readFun: Input => T, writeFun: (Output, T) => Any, allowNull: Boolean): GenCodec[T] =
    new NullSafeCodec[T] {
      def nullable: Boolean = allowNull
      def readNonNull(input: Input): T = readFun(input)
      def writeNonNull(output: Output, value: T): Unit = writeFun(output, value)
    }

  def createNullable[T <: AnyRef](readFun: Input => T, writeFun: (Output, T) => Any): GenCodec[T] =
    createNullSafe(readFun, writeFun, allowNull = true)

  def createNonNull[T](readFun: Input => T, writeFun: (Output, T) => Any): GenCodec[T] =
    createNullSafe(readFun, writeFun, allowNull = false)

  def createList[T](readFun: ListInput => T, writeFun: (ListOutput, T) => Any, allowNull: Boolean): ListCodec[T] =
    new ListCodec[T] {
      def nullable: Boolean = allowNull
      def readList(input: ListInput): T = readFun(input)
      def writeList(output: ListOutput, value: T): Unit = writeFun(output, value)
    }

  def createNullableList[T <: AnyRef](readFun: ListInput => T, writeFun: (ListOutput, T) => Any): ListCodec[T] =
    createList(readFun, writeFun, allowNull = true)

  def createNonNullList[T](readFun: ListInput => T, writeFun: (ListOutput, T) => Any): ListCodec[T] =
    createList(readFun, writeFun, allowNull = false)

  /**
    * Helper method to manually implement a `GenCodec` that writes an object. NOTE: in most cases the easiest way to
    * have a custom object codec is to manually implement `apply` and `unapply`/`unapplySeq` methods in companion object
    * of your type or use [[fromApplyUnapplyProvider]] if the type comes from a third party code and you can't
    * modify its companion object.
    */
  def createObject[T](readFun: ObjectInput => T, writeFun: (ObjectOutput, T) => Any, allowNull: Boolean): ObjectCodec[T] =
    new ObjectCodec[T] {
      def nullable: Boolean = allowNull
      def readObject(input: ObjectInput): T = readFun(input)
      def writeObject(output: ObjectOutput, value: T): Unit = writeFun(output, value)
    }

  def createNullableObject[T <: AnyRef](readFun: ObjectInput => T, writeFun: (ObjectOutput, T) => Any): ObjectCodec[T] =
    createObject(readFun, writeFun, allowNull = true)

  def createNonNullObject[T](readFun: ObjectInput => T, writeFun: (ObjectOutput, T) => Any): ObjectCodec[T] =
    createObject(readFun, writeFun, allowNull = false)

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
    def read(input: Input): T = underlying.read(input)
    def write(output: Output, value: T): Unit = underlying.write(output, value)
  }

  trait NullSafeCodec[T] extends GenCodec[T] {
    def nullable: Boolean
    def readNonNull(input: Input): T
    def writeNonNull(output: Output, value: T): Unit

    final def write(output: Output, value: T): Unit =
      if (value == null)
        if (nullable) output.writeNull() else throw new WriteFailure("null")
      else writeNonNull(output, value)

    final def read(input: Input): T =
      if (input.isNull)
        if (nullable) input.readNull().asInstanceOf[T] else throw new ReadFailure("null")
      else readNonNull(input)
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
  trait ObjectCodec[T] extends NullSafeCodec[T] {
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
    def readObject(input: ObjectInput, outOfOrderFields: FieldValues): T

    final def readObject(input: ObjectInput): T =
      readObject(input, FieldValues.Empty)
  }

  class TransformedCodec[A, B](val wrapped: GenCodec[B], onWrite: A => B, onRead: B => A) extends GenCodec[A] {
    def read(input: Input) = onRead(wrapped.read(input))
    def write(output: Output, value: A): Unit = wrapped.write(output, onWrite(value))
  }

  def underlyingCodec(codec: GenCodec[_]): GenCodec[_] = codec match {
    case tc: TransformedCodec[_, _] => underlyingCodec(tc.wrapped)
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

  implicit lazy val NothingCodec: GenCodec[Nothing] = create[Nothing](_ => throw new ReadFailure("read Nothing"), (_, _) => throw new WriteFailure("write Nothing"))
  implicit lazy val NullCodec: GenCodec[Null] = create[Null](_.readNull(), (o, _) => o.writeNull())
  implicit lazy val UnitCodec: GenCodec[Unit] = create[Unit](_.readUnit(), (o, _) => o.writeUnit())
  implicit lazy val VoidCodec: GenCodec[Void] = create[Void](_.readNull(), (o, _) => o.writeNull())

  implicit lazy val BooleanCodec: GenCodec[Boolean] = create(_.readBoolean(), _ writeBoolean _)
  implicit lazy val CharCodec: GenCodec[Char] = create(_.readChar(), _ writeChar _)
  implicit lazy val ByteCodec: GenCodec[Byte] = create(_.readByte(), _ writeByte _)
  implicit lazy val ShortCodec: GenCodec[Short] = create(_.readShort(), _ writeShort _)
  implicit lazy val IntCodec: GenCodec[Int] = create(_.readInt(), _ writeInt _)
  implicit lazy val LongCodec: GenCodec[Long] = create(_.readLong(), _ writeLong _)
  implicit lazy val FloatCodec: GenCodec[Float] = create(_.readFloat(), _ writeFloat _)
  implicit lazy val DoubleCodec: GenCodec[Double] = create(_.readDouble(), _ writeDouble _)
  implicit lazy val BigIntCodec: GenCodec[BigInt] = createNullable(_.readBigInt(), _ writeBigInt _)
  implicit lazy val BigDecimalCodec: GenCodec[BigDecimal] = createNullable(_.readBigDecimal(), _ writeBigDecimal _)

  implicit lazy val JBooleanCodec: GenCodec[JBoolean] = createNullable(_.readBoolean(), _ writeBoolean _)
  implicit lazy val JCharacterCodec: GenCodec[JCharacter] = createNullable(_.readChar(), _ writeChar _)
  implicit lazy val JByteCodec: GenCodec[JByte] = createNullable(_.readByte(), _ writeByte _)
  implicit lazy val JShortCodec: GenCodec[JShort] = createNullable(_.readShort(), _ writeShort _)
  implicit lazy val JIntegerCodec: GenCodec[JInteger] = createNullable(_.readInt(), _ writeInt _)
  implicit lazy val JLongCodec: GenCodec[JLong] = createNullable(_.readLong(), _ writeLong _)
  implicit lazy val JFloatCodec: GenCodec[JFloat] = createNullable(_.readFloat(), _ writeFloat _)
  implicit lazy val JDoubleCodec: GenCodec[JDouble] = createNullable(_.readDouble(), _ writeDouble _)
  implicit lazy val JBigIntegerCodec: GenCodec[JBigInteger] =
    createNullable(_.readBigInt().bigInteger, (o, v) => o.writeBigInt(BigInt(v)))
  implicit lazy val JBigDecimalCodec: GenCodec[JBigDecimal] =
    createNullable(_.readBigDecimal().bigDecimal, (o, v) => o.writeBigDecimal(BigDecimal(v)))

  implicit lazy val JDateCodec: GenCodec[JDate] = createNullable(i => new JDate(i.readTimestamp()), (o, d) => o.writeTimestamp(d.getTime))
  implicit lazy val StringCodec: GenCodec[String] = createNullable(_.readString(), _ writeString _)
  implicit lazy val SymbolCodec: GenCodec[Symbol] = createNullable(i => Symbol(i.readString()), (o, s) => o.writeString(s.name))
  implicit lazy val ByteArrayCodec: GenCodec[Array[Byte]] = createNullable(_.readBinary(), _ writeBinary _)

  private implicit class TraversableOnceOps[A](private val coll: TraversableOnce[A]) extends AnyVal {
    def writeToList(lo: ListOutput)(implicit writer: GenCodec[A]): Unit =
      coll.foreach(writer.write(lo.writeElement(), _))
  }

  private implicit class ListInputOps(private val li: ListInput) extends AnyVal {
    def collectTo[A: GenCodec, C](implicit cbf: CanBuildFrom[Nothing, A, C]): C = {
      val b = cbf()
      while (li.hasNext) {
        b += read[A](li.nextElement())
      }
      b.result()
    }
  }

  private implicit class ObjectInputOps(private val oi: ObjectInput) extends AnyVal {
    def collectTo[K: GenKeyCodec, V: GenCodec, C](implicit cbf: CanBuildFrom[Nothing, (K, V), C]): C = {
      val b = cbf()
      while (oi.hasNext) {
        val fi = oi.nextField()
        b += ((GenKeyCodec.read[K](fi.fieldName), read[V](fi)))
      }
      b.result()
    }
  }

  implicit def arrayCodec[T: ClassTag : GenCodec]: GenCodec[Array[T]] =
    createNullableList[Array[T]](_.iterator(read[T]).toArray[T], (lo, arr) => arr.iterator.writeToList(lo))

  // seqCodec, setCodec, jCollectionCodec, mapCodec, jMapCodec, fallbackMapCodec and fallbackJMapCodec
  // have these weird return types (e.g. GenCodec[C[T] with BSeq[T]] instead of just GenCodec[C[T]]) because it's a
  // workaround for https://groups.google.com/forum/#!topic/scala-user/O_fkaChTtg4

  implicit def seqCodec[C[X] <: BSeq[X], T: GenCodec](
    implicit cbf: CanBuildFrom[Nothing, T, C[T]]): GenCodec[C[T] with BSeq[T]] =
    createNullableList[C[T] with BSeq[T]](_.collectTo[T, C[T]], (lo, c) => c.writeToList(lo))

  implicit def setCodec[C[X] <: BSet[X], T: GenCodec](
    implicit cbf: CanBuildFrom[Nothing, T, C[T]]): GenCodec[C[T] with BSet[T]] =
    createNullableList[C[T] with BSet[T]](_.collectTo[T, C[T]], (lo, c) => c.writeToList(lo))

  implicit def jCollectionCodec[C[X] <: JCollection[X], T: GenCodec](
    implicit cbf: JCanBuildFrom[T, C[T]]): GenCodec[C[T] with JCollection[T]] =
    createNullableList[C[T] with JCollection[T]](_.collectTo[T, C[T]], (lo, c) => c.asScala.writeToList(lo))

  implicit def mapCodec[M[X, Y] <: BMap[X, Y], K: GenKeyCodec, V: GenCodec](
    implicit cbf: CanBuildFrom[Nothing, (K, V), M[K, V]]): GenCodec[M[K, V] with BMap[K, V]] =
    createNullableObject[M[K, V] with BMap[K, V]](
      _.collectTo[K, V, M[K, V]],
      (oo, value) => value.foreach({ case (k, v) => write[V](oo.writeField(GenKeyCodec.write(k)), v) })
    )

  implicit def jMapCodec[M[X, Y] <: JMap[X, Y], K: GenKeyCodec, V: GenCodec](
    implicit cbf: JCanBuildFrom[(K, V), M[K, V]]): GenCodec[M[K, V] with JMap[K, V]] =
    createNullableObject[M[K, V] with JMap[K, V]](
      _.collectTo[K, V, M[K, V]],
      (oo, value) => value.asScala.foreach({ case (k, v) => write[V](oo.writeField(GenKeyCodec.write(k)), v) })
    )

  private def readNullThen[T](i: Input, value: T): T = {
    i.readNull()
    value
  }

  implicit def optionCodec[T: GenCodec]: GenCodec[Option[T]] = create[Option[T]](
    input =>
      if (input.legacyOptionEncoding) {
        val li = input.readList()
        val res = if (li.hasNext) Some(read[T](li.nextElement())) else None
        li.skipRemaining()
        res
      } else if (input.isNull) {
        input.readNull()
        None
      } else Some(read[T](input)),

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
    new TransformedCodec[NOpt[T], Option[T]](optionCodec[T], _.toOption, _.toNOpt)

  implicit def optCodec[T: GenCodec]: GenCodec[Opt[T]] =
    create[Opt[T]](
      i => if (i.isNull) readNullThen(i, Opt.Empty) else Opt(read[T](i)),
      (o, vo) => vo match {
        case Opt(v) => write[T](o, v)
        case Opt.Empty => o.writeNull()
      }
    )

  implicit def optArgCodec[T: GenCodec]: GenCodec[OptArg[T]] =
    new TransformedCodec[OptArg[T], Opt[T]](optCodec[T], _.toOpt, _.toOptArg)

  implicit def optRefCodec[T >: Null : GenCodec]: GenCodec[OptRef[T]] =
    new TransformedCodec[OptRef[T], Opt[T]](optCodec[T], _.toOpt, _.toOptRef)

  implicit def eitherCodec[A: GenCodec, B: GenCodec]: GenCodec[Either[A, B]] = createNullableObject(
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

  implicit def jEnumCodec[E <: Enum[E] : ClassTag]: GenCodec[E] = createNullSafe(
    in => Enum.valueOf(classTag[E].runtimeClass.asInstanceOf[Class[E]], in.readString()),
    (out, value) => out.writeString(value.name),
    allowNull = true
  )

  // Needed because of SI-9453
  @silent
  @deprecated("auto codecs will be removed", "1.28.0")
  implicit lazy val NothingAutoCodec: GenCodec.Auto[Nothing] = GenCodec.Auto[Nothing](NothingCodec)

  implicit def macroGeneratedCodec[T]: MacroGenerated[GenCodec[T]] =
  macro macros.serialization.GenCodecMacros.materializeMacroGenerated[T]
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

  @deprecated("auto codecs will be removed", "1.28.0")
  implicit def materializeAuto[T]: GenCodec.Auto[T] =
  macro macros.serialization.GenCodecMacros.materializeAuto[T]
}
