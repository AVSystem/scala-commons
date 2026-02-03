package com.avsystem.commons
package serialization

import com.avsystem.commons.meta.MacroInstances

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
abstract class HasGenCodecWithDeps[D, T](using deps: ValueOf[D], macroCodec: MacroInstances[D, (codec: GenCodec[T])]) {
  given GenCodec[T] = macroCodec(deps.value, this).codec
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

//todo: check if ?=> applicable

opaque type PolyCodec[C[_]] <: [T] => GenCodec[T] => GenCodec[C[T]] = [T] => GenCodec[T] => GenCodec[C[T]]

object PolyCodec {
  inline given [C[_]] => PolyCodec[C] = [T] =>
    (codec: GenCodec[T]) => {
      given GenCodec[T] = codec
      GenCodec.derived[C[T]]
    }
}

/**
 * Like [[HasGenCodec]] but for parameterized (generic) data types.
 */
abstract class HasPolyGenCodec[C[_]](using macroCodec: MacroInstances[Unit, (codec: PolyCodec[C])]) {
  given [T: GenCodec as codec]: GenCodec[C[T]] = macroCodec((), this).codec[T](codec)
}

/**
 * A version of [[HasPolyGenCodec]] which injects additional implicits into macro materialization. Implicits are
 * imported from an object specified with type parameter `D`. It must be a singleton object type, i.e.
 * `SomeObject.type`.
 */
abstract class HasPolyGenCodecWithDeps[D: ValueOf, C[_]](using macroCodec: MacroInstances[D, (codec: PolyCodec[C])]) {
  given [T: GenCodec as codec]: GenCodec[C[T]] = macroCodec(valueOf[D], this).codec[T](codec)
}

opaque type PolyObjectCodec[C[_]] <: [T] => GenCodec[T] => GenObjectCodec[C[T]] =
  [T] => GenCodec[T] => GenObjectCodec[C[T]]

object PolyObjectCodec {
  inline given [C[_]] => PolyObjectCodec[C] = [T] =>
    (codec: GenCodec[T]) => {
      given GenCodec[T] = codec
      GenObjectCodec.derived[C[T]]
    }
}

/**
 * Like [[HasGenObjectCodec]] but for parameterized (generic) data types.
 */
abstract class HasPolyGenObjectCodec[C[_]](using macroCodec: MacroInstances[Unit, (codec: PolyObjectCodec[C])]) {
  given [T: GenCodec as codec]: GenObjectCodec[C[T]] = macroCodec((), this).codec[T](codec)
}

/**
 * A version of [[HasPolyGenObjectCodec]] which injects additional implicits into macro materialization. Implicits are
 * imported from an object specified with type parameter `D`. It must be a singleton object type, i.e.
 * `SomeObject.type`.
 */
abstract class HasPolyGenObjectCodecWithDeps[D: ValueOf, C[_]](
  using macroCodec: MacroInstances[D, (codec: PolyObjectCodec[C])],
) {
  given [T: GenCodec as codec] => GenObjectCodec[C[T]] = macroCodec(valueOf[D], this).codec[T](codec)
}

opaque type GadtCodec[C[_]] <: [T] => () => GenCodec[C[T]] = [T] => () => GenCodec[C[T]]
object GadtCodec {
  inline given [C[_]] => GadtCodec[C] = [T] => () => GenCodec.derived[C[T]]
}

/**
 * Like [[HasPolyGenCodec]] but does not require [[GenCodec]] for the type parameter of type constructor `C`. It also
 * provides a [[GenCodec]] for wildcard, i.e. `C[_]`.
 */
abstract class HasGadtCodec[C[_]](using macroCodec: MacroInstances[Unit, (codec: GadtCodec[C])]) {
  given wildcardCodec: GenCodec[C[Any]] = macroCodec((), this).codec[Any]()
  given [T] => GenCodec[C[T]] = wildcardCodec.asInstanceOf[GenCodec[C[T]]]
}

opaque type RecursiveCodec[T] <: GenCodec[T] = GenCodec[T]

object RecursiveCodec {
  inline given [T] => RecursiveCodec[T] = GenCodec.materializeRecursively
}

/**
 * Like [[HasGenCodec]] but uses [[GenCodec.materializeRecursively]] for materialization.
 */
abstract class HasRecursiveGenCodec[T](using instances: MacroInstances[Unit, (codec: RecursiveCodec[T])]) {
  given GenCodec[T] = instances((), this).codec
}

opaque type GenKeyCodecFromTransparentWrapper[T] <: GenKeyCodec[T] = GenKeyCodec[T]

object GenKeyCodecFromTransparentWrapper {
  inline given [T] => GenKeyCodecFromTransparentWrapper[T] = GenKeyCodec.forTransparentWrapper[T]
}

/**
 * Automatically injects both [[GenCodec]] and [[GenKeyCodec]]. The type must be a case class or case class like type
 * that wraps exactly one field for which [[GenKeyCodec]] exists.
 */
abstract class HasGenAndKeyCodec[T](
  using instances: MacroInstances[Unit, (codec: GenCodec[T], keyCodec: GenKeyCodecFromTransparentWrapper[T])],
) {
  given GenCodec[T] = instances((), this).codec
  given GenKeyCodec[T] = instances((), this).keyCodec
}

opaque type AUCodec[AU, T] <: AU => GenCodec[T] = AU => GenCodec[T]

object AUCodec {
  def materialize[AU, T]: AUCodec[AU, T] = GenCodec.fromApplyUnapplyProvider[T](_)
}

/**
 * Like [[HasGenCodec]] but derives the codec from a separately provided custom object which has appropriate `apply`
 * and `unapply` (or `unapplySeq`) methods implemented. Materialization is done by
 * [[GenCodec.fromApplyUnapplyProvider]] macro. The object containing `apply` and `unapply` must be specified with
 * object singleton type passed as type parameter `AU`.
 */
abstract class HasGenCodecFromAU[AU: ValueOf, T](using instances: MacroInstances[Unit, (codec: AUCodec[AU, T])]) {
  given GenCodec[T] = instances((), this).codec(valueOf[AU])
}
