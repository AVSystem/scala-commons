package com.avsystem.commons
package serialization

import com.avsystem.commons.mirror.{name, transparent}
import com.avsystem.commons.misc.{AutoNamedEnum, NamedEnumCompanion, Opt, TypedKey}

import scala.reflect.ClassTag

object CodecTestData {
  sealed trait SealedBase derives GenCodec
  object SealedBase {
    case object CaseObject extends SealedBase
    case class CaseClass(str: String) extends SealedBase
    case class Rec(sub: Opt[SealedBase], local: Opt[Rec]) extends SealedBase

    sealed trait InnerBase extends SealedBase
    object InnerBase {
      case object InnerCaseObject extends InnerBase
      case class InnerCaseClass(str: String = "kek") extends InnerBase
    }
  }
  

  @flatten sealed trait FlatSealedBase derives GenCodec {
    def id: String
  }
  object FlatSealedBase {
    case class FirstCase(id: String, int: Int = 42) extends FlatSealedBase
    case class SecondCase(id: String, dbl: Double, moar: Double*) extends FlatSealedBase
    case object ThirdCase extends FlatSealedBase {
      def id = "third"
    }
    case class RecursiveCase(id: String, sub: Opt[FlatSealedBase]) extends FlatSealedBase
    case class LocallyRecursiveCase(id: String, sub: Opt[LocallyRecursiveCase]) extends FlatSealedBase
  }

  @flatten sealed trait TransparentFlatSealedBase derives GenCodec
  @transparent
  case class TransparentCaseWrap(thing: TransparentFlatThing) extends TransparentFlatSealedBase derives GenCodec
  case class TransparentFlatThing(num: Int, text: String) derives GenCodec

  abstract class Wrapper[Self <: Wrapper[Self]: ClassTag](private val args: Any*) { this: Self =>
    override def equals(obj: Any): Boolean = obj match {
      case other: Self => args == other.args
      case _ => false
    }
    override def hashCode(): Int = args.hashCode()
  }

  object SomeObject {
    given GenCodec[SomeObject.type] = GenCodec.derived
  }

  case class NoArgCaseClass() derives GenCodec

  case class SingleArgCaseClass(str: String) derives GenCodec

  @transparent
  case class TransparentWrapper(str: String) derives GenCodec

  @transparent case class StringId(id: String) derives GenCodec

  trait HasSomeStr {
    @name("some.str") def str: String
  }
  case class SomeCaseClass(str: String, intList: List[Int]) extends HasSomeStr derives GenCodec

  case class Stuff[T](name: String)
  object Stuff {
    given GenCodec[Stuff[?]] = GenCodec.create(
      in => new Stuff[Any](in.readSimple().readString()),
      (out, value) => out.writeSimple().writeString(value.name),
    )
  }
  case class CaseClassWithWildcard(stuff: Stuff[?]) derives GenCodec

  case class CaseClassWithOptionalFields(
    str: String,
    @optionalParam int: Opt[Int],
    @optionalParam bul: Option[Boolean],
  ) derives GenCodec
  case class VarargsCaseClass(int: Int, strings: String*) derives GenCodec

  case class OnlyVarargsCaseClass(strings: String*) derives GenCodec
  case class HasDefaults(@transientDefault int: Int = 42, @transientDefault @whenAbsent("dafuq") str: String = "kek") derives GenCodec

  sealed trait CustomList derives GenCodec
  case object CustomTail extends CustomList
  @transparent case class CustomCons(tail: CustomList) extends CustomList derives GenCodec

  sealed trait BaseExpr {
    type Value
    def value: Value
  }
  sealed abstract class Expr[T](val value: T) extends BaseExpr derives GenCodec {
    type Value = T
  }
  case class IntExpr(int: Int) extends Expr[Int](int)
  case class StringExpr(str: String) extends Expr[String](str)
  case object NullExpr extends Expr[Null](null)
  object BaseExpr {
    given GenCodec[BaseExpr] = GenCodec.derived
    given GenCodec[Expr[String]] = GenCodec.derived
    given refinedCodec[T]: GenCodec[BaseExpr { type Value = T }] = GenCodec.derived
  }

  trait RecBound[+T]
  case class RecBounded(int: Int) extends RecBound[RecBounded] derives GenCodec

  @flatten sealed trait PureGadtExpr[T] derives GenCodec
  case class StringLiteral(value: String) extends PureGadtExpr[String]
  case class IntLiteral(value: Int) extends PureGadtExpr[Int]
  case object NullLiteral extends PureGadtExpr[Null]
  case class Plus[T](lhs: PureGadtExpr[T], rhs: PureGadtExpr[T]) extends PureGadtExpr[T]

  sealed trait Tree[T] derives GenCodec
  case class Leaf[T](value: T) extends Tree[T]
  case class Branch[T](left: Tree[T], right: Tree[T]) extends Tree[T]

  sealed trait Enumz derives GenCodec
  object Enumz {
    @name("Primary")
    case object First extends Enumz
    case object Second extends Enumz
    case object Third extends Enumz
  }

  sealed trait KeyEnumz derives GenCodec
  object KeyEnumz {
    @name("Primary")
    case object First extends KeyEnumz
    case object Second extends KeyEnumz
    case object Third extends KeyEnumz
  }

  sealed abstract class SealedKey[T](using val valueCodec: GenCodec[T]) extends TypedKey[T] with AutoNamedEnum
  object SealedKey extends NamedEnumCompanion[SealedKey[?]] {
    case object StringKey extends SealedKey[String]
    case object IntKey extends SealedKey[Int]
    case object BooleanKey extends SealedKey[Boolean]

    val values: List[SealedKey[?]] = caseObjects
  }

  @flatten("kejs") sealed trait CustomizedSeal derives GenCodec
  @defaultCase(transient = true) case class CustomizedCase(str: String) extends CustomizedSeal
  case class OtherCustomCase(value: Int, flag: Boolean) extends CustomizedSeal
  case object CustomizedObjekt extends CustomizedSeal

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
  ) derives GenCodec

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
    given codec[T: GenCodec]: GenCodec[SealedRefined { type X = T }] = GenCodec.derived
  }

  case class StepOne(stepTwo: StepTwo)
  case class StepTwo(stepOne: Opt[StepOne])

  case class OuterThing(inner: InnerThing)
  case class InnerThing(recursiveThing: Opt[OuterThing])
  object OuterThing {
    given GenCodec[OuterThing] = GenCodec.materializeRecursively
  }

  @transparent
  case class ThingId(value: String) derives GenCodec

  locally {
    case class LocalStuff() derives GenCodec
  }
}
