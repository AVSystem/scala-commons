package com.avsystem.commons
package serialization

import com.avsystem.commons.meta.MacroInstances
import com.avsystem.commons.meta.MacroInstances.materializeWith

/**
 * Convenience abstract class for companion objects of types that have a [[GenCodec]]. There are many other flavors of
 * this base companion class. For example, if you want to inject additional implicits into [[GenCodec]]
 * materialization, you can use [[HasGenCodecWithDeps]], for parameterized data types you can use [[HasPolyGenCodec]],
 * etc.
 */
abstract class HasGenCodec[T](using macroCodec: MacroInstances[Unit, (codec: GenCodec[T])]) {
  given GenCodec[T] = macroCodec((), this).codec
}

/**
 * Like [[HasGenCodec]] but materializes an [[ApplyUnapplyCodec]] instead of just [[GenCodec]].
 */
abstract class HasApplyUnapplyCodec[T](using macroCodec: MacroInstances[Unit, (codec: ApplyUnapplyCodec[T])]) {
  given ApplyUnapplyCodec[T] = macroCodec((), this).codec
}

/**
 * Like [[HasGenCodec]] but materializes a [[GenObjectCodec]] instead of just [[GenCodec]].
 */
abstract class HasGenObjectCodec[T](using macroCodec: MacroInstances[Unit, (codec: GenObjectCodec[T])]) {
  given GenObjectCodec[T] = macroCodec((), this).codec
}

/**
 * A version of [[HasGenCodec]] which injects additional implicits into macro materialization. Implicits are imported
 * from an object specified with type parameter `D`. It must be a singleton object type, i.e. `SomeObject.type`.
 */
abstract class HasGenCodecWithDeps[D: ValueOf, T](using macroCodec: MacroInstances[D, (codec: GenCodec[T])]) {
  given GenCodec[T] = macroCodec(valueOf[D], this).codec
}

/**
 * A version of [[HasApplyUnapplyCodecWithDeps]] which injects additional implicits into macro materialization.
 * Implicits are imported from an object specified with type parameter `D`. It must be a singleton object type, i.e.
 * `SomeObject.type`.
 */
abstract class HasApplyUnapplyCodecWithDeps[D: ValueOf, T](
  using macroCodec: MacroInstances[D, (codec: ApplyUnapplyCodec[T])],
) {
  given ApplyUnapplyCodec[T] = macroCodec(valueOf[D], this).codec
}

/**
 * A version of [[HasGenObjectCodec]] which injects additional implicits into macro materialization. Implicits are
 * imported from an object specified with type parameter `D`. It must be a singleton object type, i.e.
 * `SomeObject.type`.
 */
abstract class HasGenObjectCodecWithDeps[D: ValueOf, T](using macroCodec: MacroInstances[D, (codec: GenObjectCodec[T])]) {
  given GenObjectCodec[T] = macroCodec(valueOf[D], this).codec
}

/**
 * Like [[HasGenCodec]] but for parameterized (generic) data types.
 */
abstract class HasPolyGenCodec[C[_]](using macroCodec: MacroInstances[Unit, (codec: [T: GenCodec] => () => GenCodec[C[T]])]) {
  given [T: GenCodec]: GenCodec[C[T]] = macroCodec((), this).codec()
}

/**
 * A version of [[HasPolyGenCodec]] which injects additional implicits into macro materialization. Implicits are
 * imported from an object specified with type parameter `D`. It must be a singleton object type, i.e.
 * `SomeObject.type`.
 */
abstract class HasPolyGenCodecWithDeps[D: ValueOf, C[_]](
  using macroCodec: MacroInstances[D, (codec: [T: GenCodec] => () => GenCodec[C[T]])],
) {
  given [T: GenCodec]: GenCodec[C[T]] = macroCodec(valueOf[D], this).codec()
}

/**
 * Like [[HasGenObjectCodec]] but for parameterized (generic) data types.
 */
abstract class HasPolyGenObjectCodec[C[_]](
  using macroCodec: MacroInstances[Unit, (codec: [T: GenCodec] => () => GenObjectCodec[C[T]])],
) {
  given [T: GenCodec]: GenObjectCodec[C[T]] = macroCodec((), this).codec()
}

/**
 * A version of [[HasPolyGenObjectCodec]] which injects additional implicits into macro materialization. Implicits are
 * imported from an object specified with type parameter `D`. It must be a singleton object type, i.e.
 * `SomeObject.type`.
 */
abstract class HasPolyGenObjectCodecWithDeps[D: ValueOf, C[_]](
  using macroCodec: MacroInstances[D, (codec: [T: GenCodec] => () => GenObjectCodec[C[T]])],
) {
  given [T: GenCodec] => GenObjectCodec[C[T]] = macroCodec(valueOf[D], this).codec()
}

/**
 * Like [[HasPolyGenCodec]] but does not require [[GenCodec]] for the type parameter of type constructor `C`. It also
 * provides a [[GenCodec]] for wildcard, i.e. `C[_]`.
 */
abstract class HasGadtCodec[C[_]](using macroCodec: MacroInstances[Unit, (codec: [T] => () => GenCodec[C[T]])]) {
  given wildcardCodec: GenCodec[C[Any]] = macroCodec((), this).codec[Any]()
  given [T] => GenCodec[C[T]] = wildcardCodec.asInstanceOf[GenCodec[C[T]]]
}

trait RecursiveCodec[T] {
  @materializeWith(GenCodec, "materializeRecursively")
  def codec: GenCodec[T]
}

/**
 * Like [[HasGenCodec]] but uses [[GenCodec.materializeRecursively]] for materialization.
 */
abstract class HasRecursiveGenCodec[T](using instances: MacroInstances[Unit, (codec: GenCodec[T])]) {
  given GenCodec[T] = instances((), this).codec
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
abstract class HasGenAndKeyCodec[T](using instances: MacroInstances[Unit, (codec: GenCodec[T], keyCodec: GenKeyCodec[T])]) {
  given GenCodec[T] = instances((), this).codec
  given GenKeyCodec[T] = instances((), this).keyCodec
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
abstract class HasGenCodecFromAU[AU: ValueOf, T](using instances: MacroInstances[Unit, (codec: AU => GenCodec[T])]) {
  given GenCodec[T] = instances((), this).codec(valueOf[AU])
}
