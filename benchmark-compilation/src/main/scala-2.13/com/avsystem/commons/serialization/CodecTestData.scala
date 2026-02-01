package com.avsystem.commons
package serialization

import com.avsystem.commons.meta.MacroInstances
import com.avsystem.commons.misc.{AutoNamedEnum, NamedEnumCompanion, Opt, TypedKey}

import scala.reflect.ClassTag

object CodecTestData {
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


  @flatten sealed trait FlatSealedBase {
    def id: String
  }
  object FlatSealedBase extends HasGenCodec[FlatSealedBase] {
    case class FirstCase(id: String, int: Int = 42) extends FlatSealedBase
    case class SecondCase(id: String, dbl: Double, moar: Double*) extends FlatSealedBase
    case object ThirdCase extends FlatSealedBase {
      def id = "third"
    }
    case class RecursiveCase(id: String, sub: Opt[FlatSealedBase]) extends FlatSealedBase
    case class LocallyRecursiveCase(id: String, sub: Opt[LocallyRecursiveCase]) extends FlatSealedBase
  }

  @flatten sealed trait TransparentFlatSealedBase
  case class TransparentCaseWrap(thing: TransparentFlatThing) extends TransparentFlatSealedBase
  object TransparentCaseWrap extends TransparentWrapperCompanion[TransparentFlatThing, TransparentCaseWrap]
  object TransparentFlatSealedBase extends HasGenCodec[TransparentFlatSealedBase]

  case class TransparentFlatThing(num: Int, text: String)
  object TransparentFlatThing extends HasApplyUnapplyCodec[TransparentFlatThing]

  abstract class Wrapper[Self <: Wrapper[Self]: ClassTag](private val args: Any*) { this: Self =>
    override def equals(obj: Any): Boolean = obj match {
      case other: Self => args == other.args
      case _ => false
    }
    override def hashCode(): Int = args.hashCode()
  }

  object SomeObject {
    implicit val codec: GenCodec[SomeObject.type] = GenCodec.materialize[SomeObject.type]
  }

  case class NoArgCaseClass()
  object NoArgCaseClass extends HasGenCodec[NoArgCaseClass]

  case class SingleArgCaseClass(str: String)
  object SingleArgCaseClass extends HasGenCodec[SingleArgCaseClass]

  @transparent
  case class TransparentWrapper(str: String)
  object TransparentWrapper extends HasGenCodec[TransparentWrapper]

  @transparent case class StringId(id: String)
  object StringId extends TransparentWrapperCompanion[String, StringId]

  trait HasSomeStr {
    @name("some.str") def str: String
  }
  case class SomeCaseClass(str: String, intList: List[Int]) extends HasSomeStr
  object SomeCaseClass extends HasGenCodec[SomeCaseClass]

  case class Stuff[T](name: String)
  object Stuff {
    implicit val codec: GenCodec[Stuff[?]] = GenCodec.create(
      in => new Stuff[Any](in.readSimple().readString()),
      (out, value) => out.writeSimple().writeString(value.name),
    )
  }
  case class CaseClassWithWildcard(stuff: Stuff[?])
  object CaseClassWithWildcard extends HasGenCodec[CaseClassWithWildcard]

  case class CaseClassWithOptionalFields(
    str: String,
    @optionalParam int: Opt[Int],
    @optionalParam bul: Option[Boolean],
  )
  object CaseClassWithOptionalFields extends HasGenCodec[CaseClassWithOptionalFields]
  case class VarargsCaseClass(int: Int, strings: String*)
  object VarargsCaseClass extends HasGenCodec[VarargsCaseClass]

  case class OnlyVarargsCaseClass(strings: String*)
  object OnlyVarargsCaseClass extends HasGenCodec[OnlyVarargsCaseClass]
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
    implicit def baseGenericCodec[T]: GenCodec[BaseExpr { type Value = T }] = GenCodec.materialize
  }
  object Expr extends HasGadtCodec[Expr]

  trait RecBound[+T]
  case class RecBounded(int: Int) extends RecBound[RecBounded]
  object RecBounded extends HasGenCodec[RecBounded]
  
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

  sealed abstract class SealedKey[T](implicit val valueCodec: GenCodec[T]) extends TypedKey[T] with AutoNamedEnum
  object SealedKey extends NamedEnumCompanion[SealedKey[_]] {
    case object StringKey extends SealedKey[String]
    case object IntKey extends SealedKey[Int]
    case object BooleanKey extends SealedKey[Boolean]

    val values: List[SealedKey[_]] = caseObjects
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
    a23: String,
  )
  object ItsOverTwentyTwo extends HasGenCodec[ItsOverTwentyTwo]

  @flatten
  sealed trait Dep
  case class DepCase(str: String) extends Dep

  sealed trait SealedRefined {
    type X
  }
  object SealedRefined {
    final case class First[Type](foo: Type) extends SealedRefined {
      type X = Type
    }
    implicit def codec[T: GenCodec]: GenCodec[SealedRefined { type X = T }] = GenCodec.materialize
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
