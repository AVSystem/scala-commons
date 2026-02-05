package com.avsystem.commons
package serialization

import com.avsystem.commons.annotation.explicitGenerics
import com.avsystem.commons.derivation.DeferredInstance
import com.avsystem.commons.jiop.JFactory
import com.avsystem.commons.meta.{AllowDerivation, AllowRecursiveDerivation, Fallback}
import com.avsystem.commons.misc.{Bytes, HasAnnotation, Timestamp}

import java.util.UUID
import scala.NamedTuple.*
import scala.annotation.{implicitNotFound, tailrec, targetName}
import scala.collection.{Factory, mutable}

/**
 * Type class for types that can be serialized to [[Output]] (format-agnostic "output stream") and deserialized from
 * [[Input]] (format-agnostic "input stream"). `GenCodec` is supposed to capture generic structure of serialized
 * objects, without being bound to particular format like JSON. The actual format is determined by implementation of
 * [[Input]] and [[Output]].
 *
 * There are convenient macros for automatic derivation of [[GenCodec]] instances (`materialize` and
 * `materializeRecursively`). However, [[GenCodec]] instances still need to be explicitly declared and won't be derived
 * "automagically".
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
   * Transforms this codec into a codec of other type using a bidirectional conversion between the original and new
   * type.
   */
  final def transform[U](onWrite: U => T, onRead: T => U): GenCodec[U] =
    new GenCodec.Transformed[U, T](this, onWrite, onRead)
}

object GenCodec extends GenCodecMacros {
  final val DefaultCaseField = "_case"
  def apply[T](using codec: GenCodec[T]): GenCodec[T] = codec

  inline def derived[T]: GenCodec[T] = {
    given deferred: Deferred[T] = new Deferred[T]
    val underlying = unsafeDerived[T]
    deferred.underlying = underlying
    underlying
  }
  inline def materializeRecursively[T]: GenCodec[T] = {
    given AllowRecursiveDerivation.type = AllowRecursiveDerivation
    derived[T]
  }
  inline given [Tup <: Tuple] => GenCodec[Tup] = mkTupleCodec(
    compiletime.summonAll[Tuple.Map[Tup, GenCodec]],
  )

  inline given [NT <: AnyNamedTuple] => GenCodec[NT] =
    deriveProduct[DropNames[NT]](compiletime.summonInline[Mirror.ProductOf[DropNames[NT]]])(
      constName[NT](compiletime.summonInline[TypeRepr[NT]]),
      compiletime.summonAll[Tuple.Map[DropNames[NT], GenCodec]],
      constNames[Tuple.Zip[Names[NT], DropNames[NT]]],
    ).asInstanceOf[GenCodec[NT]]

  def materialize[T]: GenCodec[T] = ???
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
  def createNullable[T](readFun: Input => T, writeFun: (Output, T) => Any): GenCodec[T | Null] = new NullableCodec[T] {
    def readNonNull(input: Input): T = readFun(input)
    def writeNonNull(output: Output, value: T): Unit = writeFun(output, value)
  }
  def createString[T](readFun: String => T, writeFun: T => String): GenCodec[T] =
    createSimple(i => readFun(i.readString()), (o, v) => o.writeString(writeFun(v)))
  def createSimple[T](readFun: SimpleInput => T, writeFun: (SimpleOutput, T) => Any): GenCodec[T] = new SimpleCodec[T] {
    def readSimple(input: SimpleInput): T = readFun(input)
    def writeSimple(output: SimpleOutput, value: T): Unit = writeFun(output, value)
  }
  def createList[T](readFun: ListInput => T, writeFun: (ListOutput, T) => Any): GenCodec[T] = new ListCodec[T] {
    def readList(input: ListInput): T = readFun(input)
    def writeList(output: ListOutput, value: T): Unit = writeFun(output, value)
  }

  /**
   * Helper method to manually implement a `GenCodec` that writes an object. NOTE: in most cases the easiest way to
   * have a custom object codec is to manually implement `apply` and `unapply`/`unapplySeq` methods in companion object
   * of your type or use [[fromApplyUnapplyProvider]] if the type comes from a third party code and you can't modify
   * its companion object.
   */
  def createObject[T](readFun: ObjectInput => T, writeFun: (ObjectOutput, T) => Any): GenObjectCodec[T] =
    new ObjectCodec[T] {
      def readObject(input: ObjectInput): T = readFun(input)
      def writeObject(output: ObjectOutput, value: T): Unit = writeFun(output, value)
    }
  def fromKeyCodec[T](using keyCodec: GenKeyCodec[T]): GenCodec[T] = create(
    input => keyCodec.read(input.readSimple().readString()),
    (output, value) => output.writeSimple().writeString(keyCodec.write(value)),
  )
  def underlyingCodec(codec: GenCodec[?]): GenCodec[?] = codec match {
    case tc: Transformed[_, _] => underlyingCodec(tc.wrapped)
    case _ => codec
  }
  given GenCodec[Nothing] =
    create[Nothing](_ => throw new ReadFailure("read Nothing"), (_, _) => throw new WriteFailure("write Nothing"))
  given GenCodec[Null] =
    create[Null](i => if (i.readNull()) null else notNull, (o, _) => o.writeNull())
  given GenCodec[Unit] =
    create[Unit](i => if (i.readNull()) () else notNull, (o, _) => o.writeNull())
  given GenCodec[Void] =
    create[Void](i => if (i.readNull()) null.asInstanceOf[Void] else notNull, (o, _) => o.writeNull())
  given GenCodec[Boolean] = createSimple(_.readBoolean(), _ `writeBoolean` _)
  given GenCodec[Char] = createSimple(_.readChar(), _ `writeChar` _)
  given GenCodec[Byte] = createSimple(_.readByte(), _ `writeByte` _)
  given GenCodec[Short] = createSimple(_.readShort(), _ `writeShort` _)
  given GenCodec[Int] = createSimple(_.readInt(), _ `writeInt` _)
  given GenCodec[Long] = createSimple(_.readLong(), _ `writeLong` _)
  given GenCodec[Float] = createSimple(_.readFloat(), _ `writeFloat` _)
  given GenCodec[Double] = createSimple(_.readDouble(), _ `writeDouble` _)
  given GenCodec[BigInt] = createSimple(_.readBigInt(), _ `writeBigInt` _)
  given GenCodec[BigDecimal] = createSimple(_.readBigDecimal(), _ `writeBigDecimal` _)
  given GenCodec[JBoolean] = createSimple(_.readBoolean(), _ `writeBoolean` _)
  given GenCodec[JCharacter] = createSimple(_.readChar(), _ `writeChar` _)
  given GenCodec[JByte] = createSimple(_.readByte(), _ `writeByte` _)
  given GenCodec[JShort] = createSimple(_.readShort(), _ `writeShort` _)
  given GenCodec[JInteger] = createSimple(_.readInt(), _ `writeInt` _)
  given GenCodec[JLong] = createSimple(_.readLong(), _ `writeLong` _)
  given GenCodec[JFloat] = createSimple(_.readFloat(), _ `writeFloat` _)
  given GenCodec[JDouble] = createSimple(_.readDouble(), _ `writeDouble` _)
  given GenCodec[JBigInteger] = createSimple(_.readBigInt().bigInteger, (o, v) => o.writeBigInt(BigInt(v)))
  given GenCodec[JBigDecimal] = createSimple(_.readBigDecimal().bigDecimal, (o, v) => o.writeBigDecimal(BigDecimal(v)))
  given GenCodec[JDate] = createSimple(i => new JDate(i.readTimestamp()), (o, d) => o.writeTimestamp(d.getTime))
  given GenCodec[String] = createSimple(_.readString(), _ `writeString` _)
  given GenCodec[Symbol] = createSimple(i => Symbol(i.readString()), (o, s) => o.writeString(s.name))
  given GenCodec[Array[Byte]] = createSimple(_.readBinary(), _ `writeBinary` _)
  given GenCodec[UUID] = createSimple(i => UUID.fromString(i.readString()), (o, v) => o.writeString(v.toString))
  given GenCodec[Timestamp] =
    GenCodec.createSimple(i => Timestamp(i.readTimestamp()), (o, t) => o.writeTimestamp(t.millis))
  given GenCodec[Bytes] = GenCodec.createSimple(i => Bytes(i.readBinary()), (o, b) => o.writeBinary(b.bytes))
  given [T] => (codec: GenCodec[T]) => GenCodec[T | Null] = createNullable(codec.read, codec.write)
  given [T: {ClassTag, GenCodec}] => GenCodec[Array[T]] = createList[Array[T]](
    _.iterator(read[T]).toArray[T],
    (lo, arr) => {
      lo.declareSize(arr.length)
      @tailrec def loop(idx: Int): Unit =
        if (idx < arr.length) {
          GenCodec.write(lo.writeElement(), arr(idx))
          loop(idx + 1)
        }
      loop(0)
    },
  )
  @targetName("seqCodec")
  given [C[X] <: BSeq[X], T: GenCodec] => Factory[T, C[T]] => GenCodec[C[T]] =
    createList[C[T]](_.collectTo[T, C[T]], (lo, c) => c.writeToList(lo))
  given [C[X] <: BSet[X], T: GenCodec] => Factory[T, C[T]] => GenCodec[C[T]] =
    createList[C[T]](_.collectTo[T, C[T]], (lo, c) => c.writeToList(lo))
  given [C[X] <: JCollection[X], T: GenCodec] => JFactory[T, C[T]] => GenCodec[C[T]] =
    createList[C[T]](_.collectTo[T, C[T]], (lo, c) => c.asScala.writeToList(lo))
  given [M[X, Y] <: BMap[X, Y], K: GenKeyCodec, V: GenCodec] => Factory[(K, V), M[K, V]] => GenObjectCodec[M[K, V]] =
    createObject[M[K, V]](_.collectTo[K, V, M[K, V]], (oo, value) => value.writeToObject(oo))
  given [M[X, Y] <: JMap[X, Y], K: GenKeyCodec, V: GenCodec] => JFactory[(K, V), M[K, V]] => GenObjectCodec[M[K, V]] =
    createObject[M[K, V]](_.collectTo[K, V, M[K, V]], (oo, value) => value.asScala.writeToObject(oo))
  given [T: GenCodec] => GenCodec[Option[T]] = create[Option[T]](
    input =>
      if (input.legacyOptionEncoding) {
        val li = input.readList()
        val res = if (li.hasNext) Some(read[T](li.nextElement())) else None
        li.skipRemaining()
        res
      } else if (input.readNull()) None
      else Some(read[T](input)),
    (output, valueOption) =>
      if (output.legacyOptionEncoding) {
        val lo = output.writeList()
        valueOption.foreach(v => write[T](lo.writeElement(), v))
        lo.finish()
      } else
        valueOption match {
          case Some(v) => write[T](output, v)
          case None => output.writeNull()
        },
  )
  given [T: GenCodec] => GenCodec[NOpt[T]] =
    new Transformed[NOpt[T], Option[T]](summon, _.toOption, _.toNOpt)
  given [T: GenCodec] => GenCodec[Opt[T]] =
    create[Opt[T]](
      i => if (i.readNull()) Opt.Empty else Opt(read[T](i)),
      (o, vo) =>
        vo match {
          case Opt(v) => write[T](o, v)
          case Opt.Empty => o.writeNull()
        },
    )
  given [T: GenCodec] => GenCodec[OptArg[T]] =
    new Transformed[OptArg[T], Opt[T]](summon, _.toOpt, _.toOptArg)
  given [T: GenCodec] => GenCodec[OptRef[T]] =
    new Transformed[OptRef[T], Opt[T]](summon, _.toOpt, _.toOptRef)
  given [A: GenCodec, B: GenCodec] => GenCodec[Either[A, B]] = createObject(
    oi => {
      val fi = oi.nextField()
      fi.fieldName match {
        case "Left" => Left(read[A](fi))
        case "Right" => Right(read[B](fi))
        case name => throw new ReadFailure(s"Expected field 'Left' or 'Right', got $name")
      }
    },
    (oo, v) => {
      oo.declareSize(1)
      v match {
        case Left(a) => write[A](oo.writeField("Left"), a)
        case Right(b) => write[B](oo.writeField("Right"), b)
      }
    },
  )
  given [E <: Enum[E]: ClassTag] => GenCodec[E] = createSimple(
    in => Enum.valueOf(classTag[E].runtimeClass.asInstanceOf[Class[E]], in.readString()),
    (out, value) => out.writeString(value.name),
  )
  // Warning! Changing the order of implicit params of this method causes divergent implicit expansion (WTF?)
  given [R, T] => (tw: TransparentWrapping[R, T]) => (wrappedCodec: GenCodec[R]) => GenCodec[T] =
    new Transformed(wrappedCodec, tw.unwrap, tw.wrap)
  given [T] => (fallback: Fallback[GenCodec[T]]) => GenCodec[T] =
    fallback.value
  inline def fromJavaBuilder[T, B](inline newBuilder: B)(inline build: B => T): GenCodec[T] =
    ${ fromJavaBuilderImpl[T, B]('{ newBuilder }, '{ build }) }
  inline given [T] => (AllowRecursiveDerivation.type) => GenCodec[T] = GenCodec.derived[T]
  inline given [T] => (AllowDerivation[GenCodec[T]]) => GenCodec[T] = GenCodec.derived[T]
  inline private def unsafeDerived[T]: GenCodec[T] = compiletime.summonFrom {
    case _: HasAnnotation[`transparent`, T] =>
      deriveTransparentWrapper[T]
    case v: ValueOf[T] =>
      deriveSingleton(constName[T](compiletime.summonInline[TypeRepr[T]]), v.value.asInstanceOf[T & Singleton])
        .asInstanceOf[GenCodec[T]]
    case m: Mirror.ProductOf[T & Product] =>
      deriveProduct(m)(
        constName[T](compiletime.summonInline[TypeRepr[T]]),
        summonInstances[m.MirroredElemTypes](summonAllowed = true, deriveAllowed = false),
        constNames[Tuple.Zip[m.MirroredElemLabels, m.MirroredElemTypes]],
      ).asInstanceOf[GenCodec[T]]
    case m: Mirror.SumOf[T] =>
      compiletime.summonFrom {
        case f: HasAnnotation[`flatten`, T] =>
          deriveFlattenSum(m)(
            constName[T](compiletime.summonInline[TypeRepr[T]]),
            summonInstances[m.MirroredElemTypes](summonAllowed = false, deriveAllowed = true),
            constNames[Tuple.Zip[m.MirroredElemLabels, m.MirroredElemTypes]],
            f.caseFieldName,
            compiletime.summonAll[Tuple.Map[m.MirroredElemTypes, ClassTag]],
          )
        case _ =>
          deriveNestedSum(m)(
            constName[T](compiletime.summonInline[TypeRepr[T]]),
            summonInstances[m.MirroredElemTypes](summonAllowed = false, deriveAllowed = true),
            constNames[Tuple.Zip[m.MirroredElemLabels, m.MirroredElemTypes]],
            compiletime.summonAll[Tuple.Map[m.MirroredElemTypes, ClassTag]],
          )
      }
    case _: (T <:< AnyVal) =>
      deriveTransparentWrapper[T]
    case _ => raiseCannotDerivedTypeFor[GenCodec, T]
  }

  inline private def summonInstances[Elems <: Tuple](
    summonAllowed: Boolean,
    deriveAllowed: Boolean,
  ): Tuple.Map[Elems, GenCodec] =
    inline compiletime.erasedValue[Elems] match {
      case _: (elem *: elems) =>
        val elemCodec = compiletime.summonFrom {
          case codec: GenCodec[`elem`] if summonAllowed => codec
          case _ if deriveAllowed => derived[elem]
          case _: AllowRecursiveDerivation.type => derived[elem]
        }
        (elemCodec *: summonInstances[elems](summonAllowed, deriveAllowed)).asInstanceOf[Tuple.Map[Elems, GenCodec]]
      case _: EmptyTuple => EmptyTuple.asInstanceOf[Tuple.Map[Elems, GenCodec]]
    }
  inline private def deriveTransparentWrapper[T]: GenCodec[T] = ${ deriveTransparentWrapperImpl[T] }
  private def deriveTransparentWrapperImpl[T: Type](using quotes: Quotes): Expr[GenCodec[T]] = {
    import quotes.reflect.*
    TypeRepr.of[T].typeSymbol.caseFields match {
      case field :: Nil =>
        field.termRef.widen.asType match {
          case '[fieldType] =>
            val unwrapExpr = Lambda(
              Symbol.spliceOwner,
              MethodType(List("x"))(_ => List(TypeRepr.of[T]), _ => TypeRepr.of[fieldType]),
              (sym, args) => args.head.asInstanceOf[Term].select(field),
            ).asExprOf[T => fieldType]

            val wrapExpr = Lambda(
              Symbol.spliceOwner,
              MethodType(List("x"))(_ => List(TypeRepr.of[fieldType]), _ => TypeRepr.of[T]),
              (sym, args) =>
                New(TypeTree.of[T])
                  .select(TypeRepr.of[T].typeSymbol.primaryConstructor)
                  .appliedTo(args.head.asInstanceOf[Term]),
            ).asExprOf[fieldType => T]

            '{
              new Transformed[T, fieldType](
                makeLazy(compiletime.summonInline[GenCodec[fieldType]]),
                $unwrapExpr,
                $wrapExpr,
              )
            }
        }

      case _ =>
        report.errorAndAbort(s"Transparent wrapper ${TypeRepr.of[T]} must have exactly one field")
    }
  }
  private def deriveSingleton[T <: Singleton](typeRepr: String, value: T): GenCodec[T] =
    new SingletonCodec[T](typeRepr, value)
  private def mkTupleCodec[Tup <: Tuple](elementCodecs: Tuple.Map[Tup, GenCodec]): GenCodec[Tup] = new ListCodec[Tup] {
    override def readList(input: ListInput): Tup =
      elementCodecs
        .map[[X] =>> Any]([C] => (codec: C) => codec.asInstanceOf[GenCodec[?]].read(input.nextElement()))
        .asInstanceOf[Tup]

    override def writeList(output: ListOutput, value: Tup): Unit = {
      output.declareSize(elementCodecs.size)
      elementCodecs
        .zip(value)
        .map[[X] =>> Unit]([CE] =>
          (ce: CE) =>
            ce match {
              case (codec: GenCodec[Any] @unchecked, element) => codec.write(output.writeElement(), element)
            },
        )

    }
  }
  private def deriveFlattenSum[T](
    m: Mirror.SumOf[T],
  )(
    typeRepr: String,
    instances: Tuple.Map[m.MirroredElemTypes, GenCodec],
    fieldNames: Tuple,
    caseFieldName: String,
    classes: Tuple.Map[m.MirroredElemTypes, ClassTag],
  ): GenCodec[T] =
    new FlatSealedHierarchyCodec[T](
      typeRepr,
      fieldNames.toArrayOf[String],
      classes.toArrayOf[ClassTag[?]].map(_.runtimeClass),
      fieldNames.toArrayOf[String],
      fieldNames.toArrayOf[String].toSet,
      caseFieldName,
      0,
      false,
    ) {
      override def oooDependencies: Array[GenCodec[?]] = instances.toArrayOf[GenCodec[?]]
      override def caseDependencies: Array[OOOFieldsObjectCodec[?]] =
        instances.toArrayOf[OOOFieldsObjectCodec[?]]
    }
  private def deriveNestedSum[T](
    m: Mirror.SumOf[T],
  )(
    typeRepr: String,
    instances: Tuple.Map[m.MirroredElemTypes, GenCodec],
    fieldNames: Tuple,
    classes: Tuple.Map[m.MirroredElemTypes, ClassTag],
  ): GenCodec[T] = new NestedSealedHierarchyCodec[T](
    typeRepr,
    fieldNames.toArrayOf[String],
    classes.toArrayOf[ClassTag[?]].map(_.runtimeClass),
  ) {
    override def caseDependencies: Array[GenCodec[?]] = instances.toArrayOf[GenCodec[?]]
  }
  private def deriveProduct[T <: Product](
    m: Mirror.ProductOf[T],
  )(
    typeRepr: String,
    instances: Tuple,
    fieldNames: Tuple,
  ): GenCodec[T] =
    new ProductCodec[T](typeRepr, fieldNames.toArrayOf[String]) {
      override protected val dependencies: Array[GenCodec[?]] = instances.toArrayOf[GenCodec[?]]
      override protected def instantiate(fieldValues: FieldValues): T =
        m.fromProduct(
          Tuple
            .fromArray[Any](Array.range(0, dependencies.length).map(getField(fieldValues, _)))
            .asInstanceOf[m.MirroredElemTypes],
        )
    }
  private def notNull = throw new ReadFailure("not null")
  private def fromJavaBuilderImpl[T: Type, B: Type](
    newBuilder: Expr[B],
    build: Expr[B => T],
  )(using quotes: Quotes,
  ): Expr[GenCodec[T]] = {
    import quotes.reflect.*

    extension (ms: Symbol) {
      private def isJavaGetter: Boolean =
        ms.paramSymss.exists(_.exists(_.isTypeParam)) && ms.paramSymss == List(Nil) && {
          val expectedPrefix = if (ms.typeRef =:= TypeRepr.of[Boolean]) "is" else "get"
          ms.name.startsWith(expectedPrefix) && ms.name.length > expectedPrefix.length &&
          ms.name.charAt(expectedPrefix.length).isUpper
        }

      private def isJavaSetter: Boolean =
        ms.paramSymss.exists(_.exists(_.isTypeParam)) && ms.paramSymss.map(_.length) == List(1) && {
          ms.name.startsWith("set") && ms.name.length > 3 && ms.name.charAt(3).isUpper
        }
    }

    val fieldNames = new mutable.ListBuffer[Expr[String]]
    val getters = new mutable.ListBuffer[Expr[T => Any]]
    val setters = new mutable.ListBuffer[Expr[(B, Any) => B]]
    val deps = new mutable.ListBuffer[Expr[GenCodec[?]]]

    TypeRepr.of[T].typeSymbol.declaredMethods.iterator.foreach {
      case getter if getter.isDefDef && getter.isJavaGetter =>
        getter.typeRef.asType match {
          case '[propType] =>
            val getterName = getter.name
            val setterName = getterName.replaceFirst("^(get|is)", "set")

            val setterOpt = TypeRepr.of[B].typeSymbol.methodMember(setterName).head.allOverriddenSymbols.find { s =>
              s.isDefDef && s.isJavaSetter && s.paramSymss.head.head.typeRef =:= TypeRepr.of[propType]
            }
            setterOpt.foreach { setter =>
              val propName = setterName.charAt(3).toLower.toString + setterName.drop(4)
              fieldNames += Expr(propName)
              deps +=
                Expr
                  .summon[GenCodec[propType]]
                  .getOrElse(
                    throw new RuntimeException(
                      s"Cannot materialize GenCodec for ${Type.show[T]} because of problem with property $propName:\n",
                    ),
                  )
              getters += Lambda(
                Symbol.spliceOwner,
                MethodType(List("v"))(_ => List(TypeRepr.of[T]), _ => TypeRepr.of[propType]),
                (sym, args) => args.head.asInstanceOf[Term].select(getter),
              ).asExprOf[T => propType]

              setters += Lambda(
                Symbol.spliceOwner,
                MethodType(List("b", "v"))(_ => List(TypeRepr.of[B], TypeRepr.of[Any]), _ => TypeRepr.of[B]),
                (sym, args) => {
                  given Quotes = sym.asQuotes
                  val Seq(b: Term, v: Term) = args.runtimeChecked
                  b.select(setter).appliedTo('{ ${ v.asExpr }.asInstanceOf[propType] }.asTerm)
                },
              ).asExprOf[(B, Any) => B]
            }
        }
      case _ =>
    }

    '{
      new JavaBuilderBasedCodec[T, B](
        compiletime.summonInline[com.avsystem.commons.serialization.TypeRepr[T]],
        $newBuilder,
        $build,
        ${ Expr.ofList(fieldNames.result()) }.toArray,
        ${ Expr.ofList(getters.result()) }.toArray,
        ${ Expr.ofList(setters.result()) }.toArray,
      ) {
        override val dependencies = ${ Expr.ofList(deps.result()) }.toArray
      }
    }
  }
  trait NullableCodec[T] extends GenCodec[T | Null] {
    override final def write(output: Output, value: T | Null): Unit =
      if (value == null) output.writeNull()
      else writeNonNull(output, value)
    override final def read(input: Input): T | Null =
      if (input.readNull()) null
      else readNonNull(input)
    def readNonNull(input: Input): T
    def writeNonNull(output: Output, value: T): Unit
  }
  trait SimpleCodec[T] extends GenCodec[T] {
    def readSimple(input: SimpleInput): T
    def writeSimple(output: SimpleOutput, value: T): Unit

    final def write(output: Output, value: T): Unit =
      writeSimple(output.writeSimple(), value)

    final def read(input: Input): T =
      readSimple(input.readSimple())
  }
  trait ListCodec[T] extends GenCodec[T] {
    def readList(input: ListInput): T
    def writeList(output: ListOutput, value: T): Unit

    final def write(output: Output, value: T): Unit = {
      val lo = output.writeList()
      writeList(lo, value)
      lo.finish()
    }
    final def read(input: Input): T = {
      val li = input.readList()
      val result = readList(li)
      li.skipRemaining()
      result
    }
  }

  /**
   * Convenience base class for `GenCodec`s that serialize values as objects. NOTE: if you need to implement a custom
   * `GenCodec` that writes an object, the best way to do it is to have manually implemented `apply` and `unapply` in
   * companion object or by using [[GenCodec.fromApplyUnapplyProvider]].
   */
  trait ObjectCodec[T] extends GenObjectCodec[T] {
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
  trait SizedCodec[T] extends GenCodec[T] {
    def size(value: T): Int = size(value, Opt.Empty)

    def size(value: T, output: Opt[SequentialOutput]): Int

    protected final def declareSizeFor(output: SequentialOutput, value: T): Unit =
      if (output.sizePolicy != SizePolicy.Ignored) {
        output.declareSize(size(value, output.opt))
      }
  }
  trait OOOFieldsObjectCodec[T] extends ObjectCodec[T] with SizedCodec[T] {
    def readObject(input: ObjectInput, outOfOrderFields: FieldValues): T
    def writeFields(output: ObjectOutput, value: T): Unit

    final def readObject(input: ObjectInput): T =
      readObject(input, FieldValues.Empty)

    final def writeObject(output: ObjectOutput, value: T): Unit = {
      declareSizeFor(output, value)
      writeFields(output, value)
    }
  }
  class ReadFailure(msg: String, cause: Throwable | Null) extends RuntimeException(msg, cause) {
    def this(msg: String) = this(msg, null)

    override def fillInStackTrace(): Throwable =
      if (cause == null) super.fillInStackTrace() else this
  }
  case class MissingField(typeRepr: String, fieldName: String)
    extends ReadFailure(s"Cannot read $typeRepr, field $fieldName is missing in decoded data")
  case class UnknownCase(typeRepr: String, caseName: String)
    extends ReadFailure(s"Cannot read $typeRepr, unknown case: $caseName")

  extension [A](coll: BIterable[A]) {
    def writeToList(lo: ListOutput)(using writer: GenCodec[A]): Unit = {
      lo.declareSizeOf(coll)
      coll.foreach(new (A => Unit) {
        private var idx = 0
        def apply(a: A): Unit = {
          try writer.write(lo.writeElement(), a)
          catch {
            case NonFatal(e) => throw ListElementWriteFailed(idx, e)
          }
          idx += 1
        }
      })
    }
  }

  extension [A, B](coll: BIterable[(A, B)]) {
    def writeToObject(oo: ObjectOutput)(using keyWriter: GenKeyCodec[A], writer: GenCodec[B]): Unit = {
      oo.declareSizeOf(coll)
      coll.foreach { case (key, value) =>
        val fieldName = keyWriter.write(key)
        try writer.write(oo.writeField(fieldName), value)
        catch {
          case NonFatal(e) => throw MapFieldWriteFailed(fieldName, e)
        }
      }
    }
  }

  extension (li: ListInput) {
    def collectTo[A: GenCodec, C](using fac: Factory[A, C]): C = {
      val b = fac.newBuilder
      li.knownSize match {
        case -1 =>
        case size => b.sizeHint(size)
      }
      var idx = 0
      while (li.hasNext) {
        val a =
          try read[A](li.nextElement())
          catch {
            case NonFatal(e) => throw ListElementReadFailed(idx, e)
          }
        b += a
        idx += 1
      }
      b.result()
    }
  }

  extension (oi: ObjectInput) {
    def collectTo[K: GenKeyCodec, V: GenCodec, C](using fac: Factory[(K, V), C]): C = {
      val b = fac.newBuilder
      oi.knownSize match {
        case -1 =>
        case size => b.sizeHint(size)
      }
      while (oi.hasNext) {
        val fi = oi.nextField()
        val entry =
          try (GenKeyCodec.read[K](fi.fieldName), read[V](fi))
          catch {
            case NonFatal(e) => throw MapFieldReadFailed(fi.fieldName, e)
          }
        b += entry
      }
      b.result()
    }
  }
  case class MissingCase(typeRepr: String, caseFieldName: String, fieldToRead: Opt[String])
    extends ReadFailure(fieldToRead match {
      case Opt(fr) => s"Cannot read field $fr of $typeRepr before $caseFieldName field is read"
      case Opt.Empty => s"Cannot read $typeRepr, $caseFieldName field is missing"
    })
  case class NotSingleField(typeRepr: String, empty: Boolean)
    extends ReadFailure(
      s"Cannot read $typeRepr, expected object with exactly one field but got " +
        (if (empty) "empty object" else "more than one"),
    )
  case class CaseReadFailed(typeRepr: String, caseName: String, cause: Throwable)
    extends ReadFailure(s"Failed to read case $caseName of $typeRepr", cause)
  case class FieldReadFailed(typeRepr: String, fieldName: String, cause: Throwable)
    extends ReadFailure(s"Failed to read field $fieldName of $typeRepr", cause)
  case class ListElementReadFailed(idx: Int, cause: Throwable)
    extends ReadFailure(s"Failed to read list element at index $idx", cause)
  case class MapFieldReadFailed(fieldName: String, cause: Throwable)
    extends ReadFailure(s"Failed to read map field $fieldName", cause)
  class WriteFailure(msg: String, cause: Throwable | Null) extends RuntimeException(msg, cause) {
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
  final class Transformed[A, B](val wrapped: GenCodec[B], onWrite: A => B, onRead: B => A) extends GenCodec[A] {
    def read(input: Input): A = {
      val wrappedValue = wrapped.read(input)
      try onRead(wrappedValue)
      catch {
        case NonFatal(cause) => throw new ReadFailure(s"onRead conversion failed", cause)
      }
    }

    def write(output: Output, value: A): Unit = {
      val wrappedValue =
        try onWrite(value)
        catch {
          case NonFatal(cause) => throw new WriteFailure(s"onWrite conversion failed", cause)
        }
      wrapped.write(output, wrappedValue)
    }
  }
  class SubclassCodec[T: ClassTag, S >: T: GenCodec] extends GenCodec[T] {
    override def read(input: Input): T = GenCodec.read[S](input) match {
      case sub: T => sub
      case v => throw new ReadFailure(s"$v is not an instance of ${classTag[T].runtimeClass}")
    }
    override def write(output: Output, value: T): Unit = GenCodec.write[S](output, value)
  }
  object OOOFieldsObjectCodec {
    given [R, T] => (tw: TransparentWrapping[R, T]) => (wrapped: OOOFieldsObjectCodec[R]) => OOOFieldsObjectCodec[T] =
      new Transformed(wrapped, tw.unwrap, tw.wrap)
    // this was introduced so that transparent wrapper cases are possible in flat sealed hierarchies
    final class Transformed[A, B](val wrapped: OOOFieldsObjectCodec[B], onWrite: A => B, onRead: B => A)
      extends OOOFieldsObjectCodec[A] {
      def size(value: A, output: Opt[SequentialOutput]): Int =
        wrapped.size(onWrite(value), output)

      def readObject(input: ObjectInput, outOfOrderFields: FieldValues): A =
        onRead(wrapped.readObject(input, outOfOrderFields))

      def writeFields(output: ObjectOutput, value: A): Unit =
        wrapped.writeFields(output, onWrite(value))
    }
  }
}
