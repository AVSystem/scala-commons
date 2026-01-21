package com.avsystem.commons
package serialization

import com.avsystem.commons.meta.MacroInstances
import com.avsystem.commons.meta.MacroInstances.materializeWith
import com.avsystem.commons.misc.ValueOf

/**
 * Convenience abstract class for companion objects of types that have a [[GenCodec]]. There are many other flavors of
 * this base companion class. For example, if you want to inject additional implicits into [[GenCodec]]
 * materialization, you can use [[HasGenCodecWithDeps]], for parameterized data types you can use [[HasPolyGenCodec]],
 * etc.
 */
abstract class HasGenCodec[T](implicit macroCodec: MacroInstances[Unit, () => GenCodec[T]]) {
  implicit val codec: GenCodec[T] = macroCodec((), this).apply()
}

/**
 * Like [[HasGenCodec]] but materializes an [[ApplyUnapplyCodec]] instead of just [[GenCodec]].
 */
abstract class HasApplyUnapplyCodec[T](implicit macroCodec: MacroInstances[Unit, () => ApplyUnapplyCodec[T]]) {
  implicit val codec: ApplyUnapplyCodec[T] = macroCodec((), this).apply()
}

/**
 * Like [[HasGenCodec]] but materializes a [[GenObjectCodec]] instead of just [[GenCodec]].
 */
abstract class HasGenObjectCodec[T](implicit macroCodec: MacroInstances[Unit, () => GenObjectCodec[T]]) {
  implicit val codec: GenObjectCodec[T] = macroCodec((), this).apply()
}

/**
 * A version of [[HasGenCodec]] which injects additional implicits into macro materialization. Implicits are imported
 * from an object specified with type parameter `D`. It must be a singleton object type, i.e. `SomeObject.type`.
 */
abstract class HasGenCodecWithDeps[D, T](implicit deps: ValueOf[D], macroCodec: MacroInstances[D, () => GenCodec[T]]) {
  implicit val codec: GenCodec[T] = macroCodec(deps.value, this).apply()
}

/**
 * A version of [[HasApplyUnapplyCodecWithDeps]] which injects additional implicits into macro materialization.
 * Implicits are imported from an object specified with type parameter `D`. It must be a singleton object type, i.e.
 * `SomeObject.type`.
 */
abstract class HasApplyUnapplyCodecWithDeps[D, T](
  implicit deps: ValueOf[D],
  macroCodec: MacroInstances[D, () => ApplyUnapplyCodec[T]],
) {
  implicit val codec: ApplyUnapplyCodec[T] = macroCodec(deps.value, this).apply()
}

/**
 * A version of [[HasGenObjectCodec]] which injects additional implicits into macro materialization. Implicits are
 * imported from an object specified with type parameter `D`. It must be a singleton object type, i.e.
 * `SomeObject.type`.
 */
abstract class HasGenObjectCodecWithDeps[D, T](
  implicit deps: ValueOf[D],
  macroCodec: MacroInstances[D, () => GenObjectCodec[T]],
) {
  implicit val codec: GenObjectCodec[T] = macroCodec(deps.value, this).apply()
}

trait PolyCodec[C[_]] {
  def codec[T: GenCodec]: GenCodec[C[T]]
}

/**
 * Like [[HasGenCodec]] but for parameterized (generic) data types.
 */
abstract class HasPolyGenCodec[C[_]](implicit macroCodec: MacroInstances[Unit, PolyCodec[C]]) {
  implicit def codec[T: GenCodec]: GenCodec[C[T]] = macroCodec((), this).codec
}

/**
 * A version of [[HasPolyGenCodec]] which injects additional implicits into macro materialization. Implicits are
 * imported from an object specified with type parameter `D`. It must be a singleton object type, i.e.
 * `SomeObject.type`.
 */
abstract class HasPolyGenCodecWithDeps[D, C[_]](
  implicit deps: ValueOf[D],
  macroCodec: MacroInstances[D, PolyCodec[C]],
) {
  implicit def codec[T: GenCodec]: GenCodec[C[T]] = macroCodec(deps.value, this).codec
}

trait PolyObjectCodec[C[_]] {
  def codec[T: GenCodec]: GenObjectCodec[C[T]]
}

/**
 * Like [[HasGenObjectCodec]] but for parameterized (generic) data types.
 */
abstract class HasPolyGenObjectCodec[C[_]](implicit macroCodec: MacroInstances[Unit, PolyObjectCodec[C]]) {
  implicit def codec[T: GenCodec]: GenObjectCodec[C[T]] = macroCodec((), this).codec
}

/**
 * A version of [[HasPolyGenObjectCodec]] which injects additional implicits into macro materialization. Implicits are
 * imported from an object specified with type parameter `D`. It must be a singleton object type, i.e.
 * `SomeObject.type`.
 */
abstract class HasPolyGenObjectCodecWithDeps[D, C[_]](
  implicit deps: ValueOf[D],
  macroCodec: MacroInstances[D, PolyObjectCodec[C]],
) {
  implicit def codec[T: GenCodec]: GenObjectCodec[C[T]] = macroCodec(deps.value, this).codec
}

trait GadtCodec[C[_]] {
  def codec[T]: GenCodec[C[T]]
}

/**
 * Like [[HasPolyGenCodec]] but does not require [[GenCodec]] for the type parameter of type constructor `C`. It also
 * provides a [[GenCodec]] for wildcard, i.e. `C[_]`.
 */
abstract class HasGadtCodec[C[_]](implicit macroCodec: MacroInstances[Unit, GadtCodec[C]]) {
  implicit lazy val wildcardCodec: GenCodec[C[Any]] = macroCodec((), this).codec[Any]
  implicit def codec[T]: GenCodec[C[T]] = wildcardCodec.asInstanceOf[GenCodec[C[T]]]
}

trait RecursiveCodec[T] {
  @materializeWith(GenCodec, "materializeRecursively")
  def codec: GenCodec[T]
}

/**
 * Like [[HasGenCodec]] but uses [[GenCodec.materializeRecursively]] for materialization.
 */
abstract class HasRecursiveGenCodec[T](implicit instances: MacroInstances[Unit, RecursiveCodec[T]]) {
  implicit lazy val codec: GenCodec[T] = instances((), this).codec
}

trait CodecWithKeyCodec[T] {
  def codec: GenCodec[T]
  @materializeWith(GenKeyCodec, "forTransparentWrapper")
  def keyCodec: GenKeyCodec[T]
}

/**
 * Automatically injects both [[GenCodec]] and [[GenKeyCodec]]. The type must be a case class or case class like type
 * that wraps exactly one field for which [[GenKeyCodec]] exists.
 */
abstract class HasGenAndKeyCodec[T](implicit instances: MacroInstances[Unit, CodecWithKeyCodec[T]]) {
  implicit lazy val codec: GenCodec[T] = instances((), this).codec
  implicit lazy val keyCodec: GenKeyCodec[T] = instances((), this).keyCodec
}

trait AUCodec[AU, T] {
  @materializeWith(GenCodec, "fromApplyUnapplyProvider")
  def codec(au: AU): GenCodec[T]
}

/**
 * Like [[HasGenCodec]] but derives the codec from a separately provided custom object which has appropriate `apply`
 * and `unapply` (or `unapplySeq`) methods implemented. Materialization is done by
 * [[GenCodec.fromApplyUnapplyProvider]] macro. The object containing `apply` and `unapply` must be specified with
 * object singleton type passed as type parameter `AU`.
 */
abstract class HasGenCodecFromAU[AU, T](
  implicit applyUnapplyProvider: ValueOf[AU],
  instances: MacroInstances[Unit, AUCodec[AU, T]],
) {
  implicit final lazy val codec: GenCodec[T] =
    instances((), this).codec(applyUnapplyProvider.value)
}
