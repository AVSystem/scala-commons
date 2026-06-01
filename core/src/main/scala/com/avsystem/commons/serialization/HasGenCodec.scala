package com.avsystem.commons
package serialization

import com.avsystem.commons.annotation.bincompat
import com.avsystem.commons.meta.MacroInstances
import com.avsystem.commons.meta.MacroInstances.materializeWith
import com.avsystem.commons.misc.ValueOf

import scala.annotation.nowarn

/** Convenience abstract class for companion objects of types that have a [[GenCodec]]. There are many other flavors of
  * this base companion class. For example, if you want to inject additional implicits into [[GenCodec]]
  * materialization, you can use [[HasGenCodecWithDeps]], for parameterized data types you can use [[HasPolyGenCodec]],
  * etc.
  */
abstract class HasGenCodec[T](implicit macroCodec: MacroInstances[Unit, (codec: GenCodec[T])]) {
  implicit val codec: GenCodec[T] = macroCodec((), this).codec
}

/** Like [[HasGenCodec]] but materializes an [[ApplyUnapplyCodec]] instead of just [[GenCodec]].
  */
abstract class HasApplyUnapplyCodec[T](implicit macroCodec: MacroInstances[Unit, (codec: ApplyUnapplyCodec[T])]) {
  implicit val codec: ApplyUnapplyCodec[T] = macroCodec((), this).codec
}

/** Like [[HasGenCodec]] but materializes a [[GenObjectCodec]] instead of just [[GenCodec]].
  */
abstract class HasGenObjectCodec[T](implicit macroCodec: MacroInstances[Unit, (codec: GenObjectCodec[T])]) {
  implicit val codec: GenObjectCodec[T] = macroCodec((), this).codec
}

/** A version of [[HasGenCodec]] which injects additional implicits into macro materialization. Implicits are imported
  * from an object specified with type parameter `D`. It must be a singleton object type, i.e. `SomeObject.type`.
  */
abstract class HasGenCodecWithDeps[D, T](
  implicit macroCodec: MacroInstances[D, (codec: GenCodec[T])],
  deps: scala.ValueOf[D],
) {
  @bincompat
  @nowarn("msg=deprecated")
  private[serialization] def this(
    applyUnapplyProvider: ValueOf[D],
    instances: MacroInstances[D, (codec: GenCodec[T])],
  ) =
    this()(instances, applyUnapplyProvider.toScala)

  implicit val codec: GenCodec[T] = macroCodec(deps.value, this).codec
}

/** A version of [[HasApplyUnapplyCodecWithDeps]] which injects additional implicits into macro materialization.
  * Implicits are imported from an object specified with type parameter `D`. It must be a singleton object type, i.e.
  * `SomeObject.type`.
  */
abstract class HasApplyUnapplyCodecWithDeps[D, T](
  implicit macroCodec: MacroInstances[D, (codec: ApplyUnapplyCodec[T])],
  deps: scala.ValueOf[D],
) {
  @bincompat
  @nowarn("msg=deprecated")
  private[serialization] def this(
    applyUnapplyProvider: ValueOf[D],
    instances: MacroInstances[D, (codec: ApplyUnapplyCodec[T])],
  ) = this()(instances, applyUnapplyProvider.toScala)

  implicit val codec: ApplyUnapplyCodec[T] = macroCodec(deps.value, this).codec
}

/** A version of [[HasGenObjectCodec]] which injects additional implicits into macro materialization. Implicits are
  * imported from an object specified with type parameter `D`. It must be a singleton object type, i.e.
  * `SomeObject.type`.
  */
abstract class HasGenObjectCodecWithDeps[D, T](
  implicit macroCodec: MacroInstances[D, (codec: GenObjectCodec[T])],
  deps: scala.ValueOf[D],
) {
  @bincompat
  @nowarn("msg=deprecated")
  private[serialization] def this(
    applyUnapplyProvider: ValueOf[D],
    instances: MacroInstances[D, (codec: GenObjectCodec[T])],
  ) = this()(instances, applyUnapplyProvider.toScala)

  implicit val codec: GenObjectCodec[T] = macroCodec(deps.value, this).codec
}

trait PolyCodec[C[_]] {
  def codec[T: GenCodec]: GenCodec[C[T]]
}

// TODO[scala3-port]: HasPolyGenCodec — reshape PolyCodec[C] to NamedTuple form (Phase 6); stubbed for slice 4.2 MacroInstances bound
abstract class HasPolyGenCodec[C[_]] {
  implicit def codec[T: GenCodec]: GenCodec[C[T]] = ???
}

// TODO[scala3-port]: HasPolyGenCodecWithDeps — reshape PolyCodec[C] to NamedTuple form (Phase 6); stubbed for slice 4.2
abstract class HasPolyGenCodecWithDeps[D, C[_]] {
  implicit def codec[T: GenCodec]: GenCodec[C[T]] = ???
}

trait PolyObjectCodec[C[_]] {
  def codec[T: GenCodec]: GenObjectCodec[C[T]]
}

// TODO[scala3-port]: HasPolyGenObjectCodec — reshape PolyObjectCodec[C] to NamedTuple form (Phase 6); stubbed for slice 4.2
abstract class HasPolyGenObjectCodec[C[_]] {
  implicit def codec[T: GenCodec]: GenObjectCodec[C[T]] = ???
}

// TODO[scala3-port]: HasPolyGenObjectCodecWithDeps — reshape PolyObjectCodec[C] to NamedTuple form (Phase 6); stubbed for slice 4.2
abstract class HasPolyGenObjectCodecWithDeps[D, C[_]] {
  implicit def codec[T: GenCodec]: GenObjectCodec[C[T]] = ???
}

trait GadtCodec[C[_]] {
  def codec[T]: GenCodec[C[T]]
}

// TODO[scala3-port]: HasGadtCodec — reshape GadtCodec[C] to NamedTuple form (Phase 6); stubbed for slice 4.2
// Earlier note: C[_] existential narrowed to C[Any] (Scala 3 forbids HKT wildcard application) (S)
abstract class HasGadtCodec[C[_]] {
  implicit lazy val wildcardCodec: GenCodec[C[Any]] = ???
  implicit def codec[T]: GenCodec[C[T]] = wildcardCodec.asInstanceOf[GenCodec[C[T]]]
}

trait RecursiveCodec[T] {
  @materializeWith(GenCodec, "materializeRecursively")
  def codec: GenCodec[T]
}

// TODO[scala3-port]: HasRecursiveGenCodec — reshape RecursiveCodec[T] to NamedTuple form (Phase 6); stubbed for slice 4.2
abstract class HasRecursiveGenCodec[T] {
  implicit lazy val codec: GenCodec[T] = ???
}

trait CodecWithKeyCodec[T] {
  def codec: GenCodec[T]
  @materializeWith(GenKeyCodec, "forTransparentWrapper")
  def keyCodec: GenKeyCodec[T]
}

// TODO[scala3-port]: HasGenAndKeyCodec — reshape CodecWithKeyCodec[T] to NamedTuple form (Phase 6); stubbed for slice 4.2
abstract class HasGenAndKeyCodec[T] {
  implicit lazy val codec: GenCodec[T] = ???
  implicit lazy val keyCodec: GenKeyCodec[T] = ???
}

trait AUCodec[AU, T] {
  @materializeWith(GenCodec, "fromApplyUnapplyProvider")
  def codec(au: AU): GenCodec[T]
}

// TODO[scala3-port]: HasGenCodecFromAU — reshape AUCodec[AU, T] to NamedTuple form (Phase 6); stubbed for slice 4.2
abstract class HasGenCodecFromAU[AU, T] {
  implicit final lazy val codec: GenCodec[T] = ???
}
