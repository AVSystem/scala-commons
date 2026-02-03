package com.avsystem.commons
package serialization

import com.avsystem.commons.annotation.AnnotationAggregate
import com.avsystem.commons.meta.{AutoOptionalParams, MacroInstances}
import com.avsystem.commons.misc.{AutoNamedEnum, NamedEnumCompanion, TypedKey}

import scala.annotation.meta.getter

object CodecTestData {
  val jArrayList: JArrayList[Int] = col(new JArrayList[Int])
  val jLinkedList: JLinkedList[Int] = col(new JLinkedList[Int])
  val jHashSet: JHashSet[Int] = col(new JHashSet[Int])
  val jLinkedHashSet: JLinkedHashSet[Int] = col(new JLinkedHashSet[Int])
  val jTreeSet: JTreeSet[Int] = col(new JTreeSet[Int])
  val jHashMap: JHashMap[String, Int] = stringMap(new JHashMap[String, Int])
  val jIntHashMap: JHashMap[Int, Int] = map(new JHashMap[Int, Int])
  val jDoubleHashMap: JHashMap[Double, Int] = doubleMap(new JHashMap[Double, Int])
  val jLinkedHashMap: JLinkedHashMap[String, Int] = stringMap(new JLinkedHashMap[String, Int])
  val some: Option[Int] = Option(42)
  val none: Option[Int] = Option.empty
  val list: List[Int] = List(1, 2, 3)
  val set: Set[Int] = Set(1, 2, 3)
  val map: Map[String, Int] = Map("1" -> 1, "2" -> 2, "3" -> 3)
  val hashMap: IHashMap[String, Int] = IHashMap("1" -> 1, "2" -> 2, "3" -> 3)
  val intMap: Map[Int, Int] = Map(1 -> 1, 2 -> 2, 3 -> 3)
  val doubleMap: Map[Double, Int] = Map(1.0 -> 1, 2.0 -> 2, 3.0 -> 3)
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
  sealed trait SealedBase
  @flatten sealed trait FlatSealedBase {
    @mongoId def id: String
    @generated
    @name("upper_id") def upperId: String = id.toUpperCase
  }
  @flatten sealed trait TransparentFlatSealedBase
  sealed trait CustomList
  sealed trait BaseExpr {
    type Value
    def value: Value
  }
  @flatten sealed trait RecExpr[+T]
  @flatten sealed trait PureGadtExpr[T]
  sealed trait Tree[T]
  sealed trait Enumz
  sealed trait KeyEnumz
  @flatten("kejs") sealed trait CustomizedSeal
  @flatten
  sealed trait Dep
  @flatten
  sealed trait HasColl
  sealed trait SealedRefined {
    type X
  }
  trait HasSomeStr {
    @name("some.str") def str: String
    @generated def someStrLen: Int = str.length
  }
  trait ApplyAndUnapply[A, B, C] {
    def apply(a: A, lb: List[B]): C = doApply(a, lb)
    def unapply(c: C): Option[(A, List[B])] = doUnapply(c)
    protected def doApply(a: A, lb: List[B]): C
    protected def doUnapply(c: C): Option[(A, List[B])]
  }
  trait RecBound[+T]
  trait GeneratorBase {
    def value: String
    @generated def upper: String = value.toUpperCase
    @generated def abstractUpper: String
  }
  sealed abstract class Expr[T](val value: T) extends BaseExpr {
    type Value = T
  }
  sealed abstract class SealedKey[T](using val valueCodec: GenCodec[T]) extends TypedKey[T] with AutoNamedEnum
  abstract class Wrapper[Self <: Wrapper[Self]: ClassTag](private val args: Any*) { this: Self =>
    override def equals(obj: Any): Boolean = obj match {
      case other: Self => args == other.args
      case _ => false
    }
    override def hashCode(): Int = args.hashCode()
  }
  case class ValueClass(str: String) extends AnyVal
  class mongoId extends AnnotationAggregate {
    @outOfOrder
    @name("_id")
    final def aggregated: List[StaticAnnotation] = reifyAggregated
  }
  case class TransparentCaseWrap(thing: TransparentFlatThing) extends TransparentFlatSealedBase
  case class TransparentFlatThing(num: Int, text: String)
  case class NoArgCaseClass()
  case class SingleArgCaseClass(str: String)
  @transparent
  case class TransparentWrapper(str: String)
  @transparent
  case class TransparentWrapperWithDependency(str: String)
  @transparent case class StringId(id: String)
  case class SomeCaseClass(str: String, intList: List[Int]) extends HasSomeStr
  case class Stuff[T](name: String)
  case class CaseClassWithWildcard(stuff: Stuff[?])
  case class CaseClassWithOptionalFields(
    str: String,
    @optionalParam int: Opt[Int],
    @optionalParam bul: Option[Boolean],
  )
  case class CaseClassWithAutoOptionalFields(
    str: String,
    int: Opt[Int],
    bul: Option[Boolean],
    nint: NOpt[Opt[Int]],
  )
  class CaseClassLike(val str: String, val intList: List[Int]) extends Wrapper[CaseClassLike](str, intList)
  class HasInheritedApply(val str: String, val intList: List[Int]) extends Wrapper[HasInheritedApply](str, intList)
  case class ThirdParty(i: Int, s: String)
  case class VarargsCaseClass(int: Int, strings: String*)
  case class OnlyVarargsCaseClass(strings: String*)
  class VarargsCaseClassLike(val str: String, val ints: Seq[Int]) extends Wrapper[VarargsCaseClassLike](str, ints)
  class OnlyVarargsCaseClassLike(val strings: Seq[String]) extends Wrapper[OnlyVarargsCaseClassLike](strings)
  case class HasDefaults(@transientDefault int: Int = 42, @transientDefault @whenAbsent("dafuq") str: String = "kek")
  @transparent case class CustomCons(tail: CustomList) extends CustomList
  case class IntExpr(int: Int) extends Expr[Int](int)
  case class StringExpr(str: String) extends Expr[String](str)
  case class RecBounded(int: Int) extends RecBound[RecBounded]
  case class IntRecExpr(int: Int) extends RecExpr[Int]
  case class StringRecExpr(str: String) extends RecExpr[String]
  case class ArbitraryRecExpr[+T](value: T) extends RecExpr[T]
  case class RecBoundedExpr[+T <: RecBound[T]](value: T) extends RecExpr[T]
  case class LazyRecExpr[+T](expr: RecExpr[T]) extends RecExpr[T]
  case class StringLiteral(value: String) extends PureGadtExpr[String]
  case class IntLiteral(value: Int) extends PureGadtExpr[Int]
  case class Plus[T](lhs: PureGadtExpr[T], rhs: PureGadtExpr[T]) extends PureGadtExpr[T]
  case class Leaf[T](value: T) extends Tree[T]
  case class Branch[T](left: Tree[T], right: Tree[T]) extends Tree[T]
  @defaultCase(transient = true) case class CustomizedCase(str: String) extends CustomizedSeal
  case class OtherCustomCase(value: Int, flag: Boolean) extends CustomizedSeal
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
    a23: String,
  )
  case class DepCase(str: String) extends Dep
  case class HasCollCase(coll: Seq[Dep]) extends HasColl
  case class StepOne(stepTwo: StepTwo)
  case class StepTwo(stepOne: Opt[StepOne])
  case class OuterThing(inner: InnerThing)
  case class InnerThing(recursiveThing: Opt[OuterThing])
  @transparent
  case class ThingId(value: String)
  case class Generator(value: String) extends GeneratorBase {
    @generated val valUpper: String = value.toUpperCase
    @(generated @getter)
    val getterUpper: String = value.toUpperCase
    @generated val lazyValUpper: String = value.toUpperCase
    @generated var varUpper: String = value.toUpperCase
    def abstractUpper: String = value.toUpperCase
  }
  object ValueClass extends HasGenCodec[ValueClass]
  object SealedBase {
    given GenCodec[SealedBase] = GenCodec.materialize[SealedBase]
    sealed trait InnerBase extends SealedBase
    case class CaseClass(str: String) extends SealedBase
    case class Rec(sub: Opt[SealedBase], local: Opt[Rec]) extends SealedBase
    case object CaseObject extends SealedBase
    object InnerBase {
      case class InnerCaseClass(str: String = "kek") extends InnerBase
      case object InnerCaseObject extends InnerBase
    }
  }
  object FlatSealedBase extends HasGenCodec[FlatSealedBase] {
    case class FirstCase(id: String, int: Int = 42) extends FlatSealedBase
    case class SecondCase(id: String, dbl: Double, moar: Double*) extends FlatSealedBase
    case class RecursiveCase(id: String, sub: Opt[FlatSealedBase]) extends FlatSealedBase
    case class LocallyRecursiveCase(id: String, sub: Opt[LocallyRecursiveCase]) extends FlatSealedBase
    case object ThirdCase extends FlatSealedBase {
      @generated def id = "third"
    }
  }
//  object TransparentCaseWrap extends TransparentWrapperCompanion[TransparentFlatThing, TransparentCaseWrap]
  object TransparentFlatSealedBase extends HasGenCodec[TransparentFlatSealedBase]
  object TransparentFlatThing extends HasApplyUnapplyCodec[TransparentFlatThing]
  object SomeObject {
    given GenCodec[SomeObject.type] = GenCodec.derived[SomeObject.type]
    @generated def random: Int = 42
  }
  
  GenCodec.derived[NoArgCaseClass]
  
  object NoArgCaseClass extends HasGenCodec[NoArgCaseClass]
  object SingleArgCaseClass extends HasGenCodec[SingleArgCaseClass]
  object TransparentWrapper extends HasGenCodec[TransparentWrapper]
  object TransparentWrapperWithDependency {
    // order matters
    given GenCodec[TransparentWrapperWithDependency] = GenCodec.materialize
    given stringCodec: GenCodec[String] = GenCodec.given_GenCodec_String
  }
//  object StringId extends TransparentWrapperCompanion[String, StringId]
  object SomeCaseClass extends HasGenCodec[SomeCaseClass]
  object Stuff {
    given GenCodec[Stuff[?]] = GenCodec.create(
      in => new Stuff[Any](in.readSimple().readString()),
      (out, value) => out.writeSimple().writeString(value.name),
    )
  }
  object CaseClassWithWildcard extends HasGenCodec[CaseClassWithWildcard]
  object CaseClassWithOptionalFields extends HasGenCodec[CaseClassWithOptionalFields]
  object CaseClassWithAutoOptionalFields
    extends HasGenCodecWithDeps[AutoOptionalParams.type, CaseClassWithAutoOptionalFields]
  object CaseClassLike extends HasGenCodec[CaseClassLike] {
    def apply(@name("some.str") str: String, intList: List[Int]): CaseClassLike = new CaseClassLike(str, intList)
    def unapply(ccl: CaseClassLike): Opt[(String, List[Int])] = (ccl.str, ccl.intList).opt
  }
  object HasInheritedApply extends HasGenCodec[HasInheritedApply] with ApplyAndUnapply[String, Int, HasInheritedApply] {
    protected def doApply(a: String, lb: List[Int]): HasInheritedApply = new HasInheritedApply(a, lb)
    protected def doUnapply(c: HasInheritedApply): Option[(String, List[Int])] = (c.str, c.intList).option
  }
  object ThirdParty extends HasGenCodecFromAU[ThirdPartyFakeCompanion.type, ThirdParty]
  object ThirdPartyFakeCompanion {
    def apply(str: String, int: Int): ThirdParty = ThirdParty(int, str)
    def unapply(tp: ThirdParty): Opt[(String, Int)] = (tp.s, tp.i).opt
  }
  object VarargsCaseClass extends HasGenCodec[VarargsCaseClass]
  object OnlyVarargsCaseClass extends HasGenCodec[OnlyVarargsCaseClass]
  object VarargsCaseClassLike extends HasGenCodec[VarargsCaseClassLike] {
    def apply(@name("some.str") str: String, ints: Int*): VarargsCaseClassLike = new VarargsCaseClassLike(str, ints)
    def unapplySeq(vccl: VarargsCaseClassLike): Opt[(String, Seq[Int])] = (vccl.str, vccl.ints).opt
  }
  object OnlyVarargsCaseClassLike extends HasGenCodec[OnlyVarargsCaseClassLike] {
    def apply(strings: String*): OnlyVarargsCaseClassLike = new OnlyVarargsCaseClassLike(strings)
    def unapplySeq(vccl: OnlyVarargsCaseClassLike): Opt[Seq[String]] = vccl.strings.opt
  }
  object HasDefaults extends HasGenCodec[HasDefaults]
  case object CustomTail extends CustomList
  object CustomCons extends HasGenCodec[CustomCons]
  object CustomList extends HasGenCodec[CustomList]
  case object NullExpr extends Expr[Null](null)
  object BaseExpr {
    given baseGenericCodec[T]: GenCodec[BaseExpr { type Value = T }] = GenCodec.materialize
    given baseCodec: GenCodec[BaseExpr] = GenCodec.materialize
    given stringCodec: GenCodec[Expr[String]] = GenCodec.materialize
  }
  object Expr extends HasGadtCodec[Expr]
  object RecBounded extends HasGenCodec[RecBounded]
  case object NothingRecExpr extends RecExpr[Nothing]
  object RecExpr {
    given [T: GenCodec] => GenCodec[RecExpr[T]] =
      mkCodec[Nothing](using GenCodec[T].asInstanceOf[GenCodec[Nothing]]).asInstanceOf[GenCodec[RecExpr[T]]]
    private def mkCodec[T <: RecBound[T]: GenCodec]: GenCodec[RecExpr[T]] = GenCodec.materialize
  }
  case object NullLiteral extends PureGadtExpr[Null]
  object PureGadtExpr extends HasGadtCodec[PureGadtExpr]
  object Tree extends HasPolyGenCodec[Tree]
  object Enumz {
    given GenCodec[Enumz] = GenCodec.materialize[Enumz]
    @name("Primary")
    case object First extends Enumz
    case object Second extends Enumz
    case object Third extends Enumz
  }
  object KeyEnumz {
    given GenCodec[KeyEnumz] = GenCodec.derived[KeyEnumz]
    @name("Primary")
    case object First extends KeyEnumz
    case object Second extends KeyEnumz
    case object Third extends KeyEnumz
  }
  object SealedKey extends NamedEnumCompanion[SealedKey[?]] {
    val values: List[SealedKey[?]] = caseObjects
    case object StringKey extends SealedKey[String]
    case object IntKey extends SealedKey[Int]
    case object BooleanKey extends SealedKey[Boolean]
  }
  case object CustomizedObjekt extends CustomizedSeal
  object CustomizedSeal extends HasGenCodec[CustomizedSeal]
  object ItsOverTwentyTwo extends HasGenCodec[ItsOverTwentyTwo]
  object HasColl extends HasRecursiveGenCodec[HasColl]
  object SealedRefined {
    given [T: GenCodec] => GenCodec[SealedRefined { type X = T }] = GenCodec.materialize
    final case class First[Type](foo: Type) extends SealedRefined {
      type X = Type
    }
  }

  locally {
    case class LocalStuff()
    object LocalStuff extends HasGenCodec[LocalStuff](using MacroInstances.materialize)
  }
  object OuterThing extends HasRecursiveGenCodec[OuterThing]
//  object ThingId extends StringWrapperCompanion[ThingId]
  object Generator extends HasGenCodec[Generator]
}
