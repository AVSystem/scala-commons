package com.avsystem.commons
package serialization

import com.avsystem.commons.annotation.AnnotationAggregate
import com.avsystem.commons.meta.MacroInstances
import com.avsystem.commons.meta.AutoOptionalParams
import com.avsystem.commons.misc.{TypedKey, TypedKeyCompanion}

object CodecTestData {
  def col[T <: JCollection[Int]](col: T): T = {
    col.add(1)
    col.add(2)
    col.add(3)
    col
  }

  def map[M <: JMap[Int, Int]](map: M): M = {
    map.put(1, 1)
    map.put(2, 2)
    map.put(3, 3)
    map
  }

  def stringMap[M <: JMap[String, Int]](map: M): M = {
    map.put("1", 1)
    map.put("2", 2)
    map.put("3", 3)
    map
  }

  def doubleMap[M <: JMap[Double, Int]](map: M): M = {
    map.put(1.0, 1)
    map.put(2.0, 2)
    map.put(3.0, 3)
    map
  }

  val jArrayList: JArrayList[Int] = col(new JArrayList[Int])
  val jLinkedList: JLinkedList[Int] = col(new JLinkedList[Int])
  val jHashSet: JHashSet[Int] = col(new JHashSet[Int])
  val jLinkedHashSet: JLinkedHashSet[Int] = col(new JLinkedHashSet[Int])
  val jTreeSet: JTreeSet[Int] = col(new JTreeSet[Int])
  val jHashMap: JHashMap[String, Int] = stringMap(new JHashMap[String, Int])
  val jIntHashMap: JHashMap[Int, Int] = map(new JHashMap[Int, Int])
  val jDoubleHashMap: JHashMap[Double, Int] = doubleMap(new JHashMap[Double, Int])
  val jLinkedHashMap: JLinkedHashMap[String, Int] = stringMap(new JLinkedHashMap[String, Int])

  val some = Option(42)
  val none = Option.empty[Int]
  val list = List(1, 2, 3)
  val set = Set(1, 2, 3)
  val map = Map("1" -> 1, "2" -> 2, "3" -> 3)
  val hashMap = IHashMap("1" -> 1, "2" -> 2, "3" -> 3)
  val intMap = Map(1 -> 1, 2 -> 2, 3 -> 3)
  val doubleMap = Map(1.0 -> 1, 2.0 -> 2, 3.0 -> 3)

  case class ValueClass(str: String) extends AnyVal
  object ValueClass extends HasGenCodec[ValueClass]

  sealed trait SealedBase
  object SealedBase {
    case object CaseObject extends SealedBase
    case class CaseClass(str: String) extends SealedBase
    case class Rec(sub: Opt[SealedBase], local: Opt[Rec]) extends SealedBase

    sealed trait InnerBase extends SealedBase
    object InnerBase {
      case object InnerCaseObject extends InnerBase
      case class InnerCaseClass(str: String = "kek") extends InnerBase
    }

    implicit val codec: GenCodec[SealedBase] = GenCodec.materialize[SealedBase]
  }

  class mongoId extends AnnotationAggregate {
    @outOfOrder @name("_id")
    final def aggregated: List[StaticAnnotation] = reifyAggregated
  }

  @flatten sealed trait FlatSealedBase {
    @mongoId def id: String
    @generated @name("upper_id") def upperId: String = id.toUpperCase
  }
  object FlatSealedBase {
    case class FirstCase(id: String, int: Int = 42) extends FlatSealedBase
    case class SecondCase(id: String, dbl: Double, moar: Double*) extends FlatSealedBase
    case object ThirdCase extends FlatSealedBase {
      @generated def id = "third"
    }
    case class RecursiveCase(id: String, sub: Opt[FlatSealedBase]) extends FlatSealedBase
    case class LocallyRecursiveCase(id: String, sub: Opt[LocallyRecursiveCase]) extends FlatSealedBase
    // for Scala 2.11
    implicit val codec: GenCodec[FlatSealedBase] = GenCodec.materialize
  }

  @flatten sealed trait TransparentFlatSealedBase
  case class TransparentCaseWrap(thing: TransparentFlatThing) extends TransparentFlatSealedBase
  object TransparentCaseWrap extends TransparentWrapperCompanion[TransparentFlatThing, TransparentCaseWrap]
  object TransparentFlatSealedBase extends HasGenCodec[TransparentFlatSealedBase]

  case class TransparentFlatThing(num: Int, text: String)
  object TransparentFlatThing extends HasApplyUnapplyCodec[TransparentFlatThing]

  abstract class Wrapper[Self <: Wrapper[Self] : ClassTag](private val args: Any*) { this: Self =>
    override def equals(obj: Any): Boolean = obj match {
      case other: Self => args == other.args
      case _ => false
    }
    override def hashCode(): Int = args.hashCode()
  }

  trait Framework {
    type Field

    case class Stuff(field: Field)
  }

  trait BetterFramework extends Framework {
    implicit def fieldCodec: GenCodec[Field]

    implicit val stuffCodec: GenCodec[Stuff] = GenCodec.materialize
  }

  object SomeObject {
    @generated def random: Int = 42
    implicit val codec: GenCodec[SomeObject.type] = GenCodec.materialize[SomeObject.type]
  }

  case class NoArgCaseClass()
  object NoArgCaseClass extends HasGenCodec[NoArgCaseClass]

  case class SingleArgCaseClass(str: String)
  object SingleArgCaseClass extends HasGenCodec[SingleArgCaseClass]

  @transparent
  case class TransparentWrapper(str: String)
  object TransparentWrapper extends HasGenCodec[TransparentWrapper]

  @transparent
  case class TransparentWrapperWithDependency(str: String)
  object TransparentWrapperWithDependency {
    //order matters
    implicit val codec: GenCodec[TransparentWrapperWithDependency] = GenCodec.materialize
    implicit val stringCodec: GenCodec[String] = GenCodec.StringCodec
  }

  @transparent case class StringId(id: String)
  object StringId extends TransparentWrapperCompanion[String, StringId]

  trait HasSomeStr {
    @name("some.str") def str: String
    @generated def someStrLen: Int = str.length
  }
  case class SomeCaseClass(str: String, intList: List[Int]) extends HasSomeStr
  object SomeCaseClass extends HasGenCodec[SomeCaseClass]

  case class Stuff[T](name: String)
  object Stuff {
    implicit val codec: GenCodec[Stuff[_]] = GenCodec.create(
      in => new Stuff[Any](in.readSimple().readString()),
      (out, value) => out.writeSimple().writeString(value.name)
    )
  }
  case class CaseClassWithWildcard(stuff: Stuff[_])
  object CaseClassWithWildcard extends HasGenCodec[CaseClassWithWildcard]

  case class CaseClassWithOptionalFields(
    str: String,
    @optionalParam int: Opt[Int],
    @optionalParam bul: Option[Boolean]
  )
  object CaseClassWithOptionalFields extends HasGenCodec[CaseClassWithOptionalFields]

  case class CaseClassWithAutoOptionalFields(
    str: String,
    int: Opt[Int],
    bul: Option[Boolean]
  )
  object CaseClassWithAutoOptionalFields extends HasGenCodecWithDeps[AutoOptionalParams.type, CaseClassWithAutoOptionalFields]

  class CaseClassLike(val str: String, val intList: List[Int])
    extends Wrapper[CaseClassLike](str, intList)
  object CaseClassLike extends HasGenCodec[CaseClassLike] {
    def apply(@name("some.str") str: String, intList: List[Int]): CaseClassLike = new CaseClassLike(str, intList)
    def unapply(ccl: CaseClassLike): Opt[(String, List[Int])] = (ccl.str, ccl.intList).opt
  }

  class HasInheritedApply(val str: String, val intList: List[Int])
    extends Wrapper[HasInheritedApply](str, intList)
  trait ApplyAndUnapply[A, B, C] {
    protected def doApply(a: A, lb: List[B]): C
    protected def doUnapply(c: C): Option[(A, List[B])]
    def apply(a: A, lb: List[B]): C = doApply(a, lb)
    def unapply(c: C): Option[(A, List[B])] = doUnapply(c)
  }
  object HasInheritedApply extends HasGenCodec[HasInheritedApply] with ApplyAndUnapply[String, Int, HasInheritedApply] {
    protected def doApply(a: String, lb: List[Int]): HasInheritedApply = new HasInheritedApply(a, lb)
    protected def doUnapply(c: HasInheritedApply): Option[(String, List[Int])] = (c.str, c.intList).option
  }

  case class ThirdParty(i: Int, s: String)
  object ThirdParty extends HasGenCodecFromAU[ThirdPartyFakeCompanion.type, ThirdParty]

  object ThirdPartyFakeCompanion {
    def apply(str: String, int: Int): ThirdParty = ThirdParty(int, str)
    def unapply(tp: ThirdParty): Opt[(String, Int)] = (tp.s, tp.i).opt
  }

  case class VarargsCaseClass(int: Int, strings: String*)
  object VarargsCaseClass extends HasGenCodec[VarargsCaseClass]

  case class OnlyVarargsCaseClass(strings: String*)
  object OnlyVarargsCaseClass extends HasGenCodec[OnlyVarargsCaseClass]

  class VarargsCaseClassLike(val str: String, val ints: Seq[Int]) extends Wrapper[VarargsCaseClassLike](str, ints)
  object VarargsCaseClassLike extends HasGenCodec[VarargsCaseClassLike] {
    def apply(@name("some.str") str: String, ints: Int*): VarargsCaseClassLike = new VarargsCaseClassLike(str, ints)
    def unapplySeq(vccl: VarargsCaseClassLike): Opt[(String, Seq[Int])] = (vccl.str, vccl.ints).opt
  }

  class OnlyVarargsCaseClassLike(val strings: Seq[String]) extends Wrapper[OnlyVarargsCaseClassLike](strings)
  object OnlyVarargsCaseClassLike extends HasGenCodec[OnlyVarargsCaseClassLike] {
    def apply(strings: String*): OnlyVarargsCaseClassLike = new OnlyVarargsCaseClassLike(strings)
    def unapplySeq(vccl: OnlyVarargsCaseClassLike): Opt[Seq[String]] = vccl.strings.opt
  }

  case class HasDefaults(@transientDefault int: Int = 42, @transientDefault @whenAbsent("dafuq") str: String = "kek")
  object HasDefaults extends HasGenCodec[HasDefaults]

  sealed trait CustomList
  case object CustomTail extends CustomList
  @transparent case class CustomCons(tail: CustomList) extends CustomList
  object CustomCons extends HasGenCodec[CustomCons]
  object CustomList extends HasGenCodec[CustomList]

  sealed trait BaseExpr {
    type Value
    def value: Value
  }
  sealed abstract class Expr[T](val value: T) extends BaseExpr {
    type Value = T
  }
  case class IntExpr(int: Int) extends Expr[Int](int)
  case class StringExpr(str: String) extends Expr[String](str)
  case object NullExpr extends Expr[Null](null)
  object BaseExpr {
    implicit val baseCodec: GenCodec[BaseExpr] = GenCodec.materialize
    implicit val stringCodec: GenCodec[Expr[String]] = GenCodec.materialize
    implicit def baseGenericCodec[T]: GenCodec[BaseExpr {type Value = T}] = GenCodec.materialize
  }
  object Expr extends HasGadtCodec[Expr]

  trait RecBound[+T]
  case class RecBounded(int: Int) extends RecBound[RecBounded]
  object RecBounded extends HasGenCodec[RecBounded]

  @flatten sealed trait RecExpr[+T]
  case class IntRecExpr(int: Int) extends RecExpr[Int]
  case class StringRecExpr(str: String) extends RecExpr[String]
  case object NothingRecExpr extends RecExpr[Nothing]
  case class ArbitraryRecExpr[+T](value: T) extends RecExpr[T]
  case class RecBoundedExpr[+T <: RecBound[T]](value: T) extends RecExpr[T]
  case class LazyRecExpr[+T](expr: RecExpr[T]) extends RecExpr[T]
  object RecExpr {
    private def mkCodec[T <: RecBound[T] : GenCodec]: GenCodec[RecExpr[T]] = GenCodec.materialize
    implicit def codec[T: GenCodec]: GenCodec[RecExpr[T]] =
      mkCodec[Nothing](GenCodec[T].asInstanceOf[GenCodec[Nothing]]).asInstanceOf[GenCodec[RecExpr[T]]]
  }

  @flatten sealed trait PureGadtExpr[T]
  case class StringLiteral(value: String) extends PureGadtExpr[String]
  case class IntLiteral(value: Int) extends PureGadtExpr[Int]
  case object NullLiteral extends PureGadtExpr[Null]
  case class Plus[T](lhs: PureGadtExpr[T], rhs: PureGadtExpr[T]) extends PureGadtExpr[T]
  object PureGadtExpr extends HasGadtCodec[PureGadtExpr]

  sealed trait Tree[T]
  case class Leaf[T](value: T) extends Tree[T]
  case class Branch[T](left: Tree[T], right: Tree[T]) extends Tree[T]
  object Tree extends HasPolyGenCodec[Tree]

  sealed trait Enumz
  object Enumz {
    @name("Primary")
    case object First extends Enumz
    case object Second extends Enumz
    case object Third extends Enumz

    implicit val codec: GenCodec[Enumz] = GenCodec.materialize[Enumz]
  }

  sealed trait KeyEnumz
  object KeyEnumz {
    @name("Primary")
    case object First extends KeyEnumz
    case object Second extends KeyEnumz
    case object Third extends KeyEnumz

    implicit val codec: GenCodec[KeyEnumz] = GenCodec.forSealedEnum[KeyEnumz]
  }

  sealed abstract class SealedKey[T: GenCodec] extends TypedKey[T]
  object SealedKey extends TypedKeyCompanion[SealedKey] {
    @name("StrKey")
    case object StringKey extends SealedKey[String]
    case object IntKey extends SealedKey[Int]
    case object BooleanKey extends SealedKey[Boolean]

    val values: List[SealedKey[_]] = caseObjects
    implicit def keyCodec: GenKeyCodec[SealedKey[_]] = GenKeyCodec.forSealedEnum[SealedKey[_]]
  }

  @flatten("kejs") sealed trait CustomizedSeal
  @defaultCase(transient = true) case class CustomizedCase(str: String) extends CustomizedSeal
  case class OtherCustomCase(value: Int, flag: Boolean) extends CustomizedSeal
  case object CustomizedObjekt extends CustomizedSeal
  object CustomizedSeal extends HasGenCodec[CustomizedSeal]

  case class ItsOverTwentyTwo(
    a1: String,
    a2: String,
    a3: String,
    a4: String,
    a5: String,
    a6: String,
    a7: String,
    a8: String,
    a9: String,
    a10: String,
    a11: String,
    a12: String,
    a13: String,
    a14: String,
    a15: String,
    a16: String,
    a17: String,
    a18: String,
    a19: String,
    a20: String,
    a21: String,
    a22: String,
    a23: String
  )
  object ItsOverTwentyTwo extends HasGenCodec[ItsOverTwentyTwo]

  @flatten
  sealed trait Dep
  case class DepCase(str: String) extends Dep

  @flatten
  sealed trait HasColl
  case class HasCollCase(coll: Seq[Dep]) extends HasColl
  object HasColl extends HasRecursiveGenCodec[HasColl]

  sealed trait SealedRefined {
    type X
  }
  object SealedRefined {
    final case class First[Type](foo: Type) extends SealedRefined {
      type X = Type
    }
    implicit def codec[T: GenCodec]: GenCodec[SealedRefined {type X = T}] = GenCodec.materialize
  }

  case class StepOne(stepTwo: StepTwo)
  case class StepTwo(stepOne: Opt[StepOne])

  case class OuterThing(inner: InnerThing)
  case class InnerThing(recursiveThing: Opt[OuterThing])
  object OuterThing extends HasRecursiveGenCodec[OuterThing]

  @transparent
  case class ThingId(value: String)
  object ThingId extends StringWrapperCompanion[ThingId]

  locally {
    case class LocalStuff()
    object LocalStuff extends HasGenCodec[LocalStuff]()(MacroInstances.materialize)
  }
}
