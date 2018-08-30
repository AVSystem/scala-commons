package com.avsystem.commons
package meta

import com.avsystem.commons.rpc.{encoded, methodName, paramTag, rpcName, tagged, verbatim}

import scala.annotation.StaticAnnotation

/**
  * For annotations applied on real RPC traits, their methods and parameters.
  */
trait RealSymAnnotation extends StaticAnnotation

/**
  * For annotations applied on raw RPC traits, their methods, parameters and also metadata parameters for
  * RPC traits and data types. They specify how real symbols are matched against raw symbols (e.g. real RPC
  * method against raw RPC methods).
  */
trait RawSymAnnotation extends StaticAnnotation
trait RawMethodAnnotation extends RawSymAnnotation
trait RawParamAnnotation extends RawSymAnnotation

/**
  * Can be applied on raw method parameters or metadata parameters. When a parameter is annotated as `@composite`,
  * the macro engine expects its type to be a class with public primary constructor. Then, it recursively inspects its
  * constructor parameters and treats them as if they were direct parameters. This effectively groups multiple
  * raw parameters or multiple metadata parameters into a single class.
  */
final class composite extends RawParamAnnotation

/**
  * Base trait for RPC arity annotations, [[single]], [[optional]] and [[multi]].
  * Arity annotations may be used in multiple contexts:
  *
  * - raw methods
  * - raw parameters
  * - metadata parameters for raw methods and raw parameters
  * - metadata parameters that reify annotations (i.e. annotated with [[reifyAnnot]])
  *
  * See documentation of each arity annotation for more details.
  */
sealed trait SymbolArity extends RawParamAnnotation

/**
  * The default arity annotation. Usually there is no need to use this annotation explicitly.
  *
  * When applied on raw method, there must be exactly one real method matching this raw method and it must have
  * the same name (or [[rpcName]]) as raw method's name.
  *
  * When applied on raw parameter, specifies that this raw parameter must be matched by exactly one real parameter,
  * on the position that matches raw parameter's position. Names are ignored because - unlike methods - parameters are
  * identified by their position in parameter list(s) rather than their name.
  *
  * By default, [[single]] raw methods and parameters are [[verbatim]] which means that the real method must have
  * exactly the same return type as raw method and real parameter must have exactly the same type as raw parameter.
  *
  * When applied on method metadata parameter or parameter metadata parameter, the rules above apply in the same way
  * except that real method/parameter type is matched against the type passed as `T` to `TypedMetadata[T]` by
  * metadata class. For example, if method metadata class extends `TypedMetadata[Unit]` then `Unit` is assumed to be
  * the result type of raw method that this metadata class represents. Similarly, when parameter metadata class
  * extends `TypedMetadata[Int]` then raw parameter represented by this metadata class is assumed to be of type `Int`.
  *
  * Finally, when applied on metadata parameter with [[reifyAnnot]], that parameter is supposed to hold exactly one
  * instance of an annotation of given type from real trait/method/param. If there is no such annotation available,
  * compilation error will be raised.
  */
final class single extends SymbolArity

/**
  * When applied on raw method, it works almost the same way as [[single]] except that it is not required that
  * matching real method exists in real RPC trait. If there is no matching real method, macro-materialized `AsRaw`
  * implementation will implement the raw method with a code that throws an exception.
  *
  * When applied on raw parameter, specifies that this raw parameter may be matched by a real parameter but this is
  * not required. Whether a real parameter matches an optional raw parameter is determined by its type and/or tag
  * (see [[paramTag]] for more information on param tagging).
  *
  * Raw parameters marked as [[optional]] must be typed as `Option[T]` (or `Opt[T]`, `OptArg[T]` or whatever type that
  * has an instance of `OptionLike`). By default, [[optional]] raw parameters are [[verbatim]] which means that the
  * option-wrapped type `T` must match exactly the type of real parameter.
  *
  * In the macro generated code that translates a raw call into a real call, when the raw parameter value is absent
  * (the `Option[T]` is empty) then real parameter's default value will be used as fallback. This allows introducing
  * new parameters into RPC interfaces without breaking backwards compatibility.
  *
  * [[optional]] may also be used on method metadata parameters or parameter metadata parameters. It works the same
  * way as with [[single]] except that metadata class must be wrapped in an `OptionLike` type (`Option`, `Opt`, etc.).
  *
  * Finally, [[optional]] may also be used for metadata parameters that hold reified annotations (see [[reifyAnnot]]).
  * In that case it is not required that the annotation being reified is actually present on the real trait/method/param.
  * For that to work, metadata param must be typed as `Option[A]`, `Opt[A]`, etc. where `A` is the type of annotation
  * being reified.
  */
final class optional extends SymbolArity

/**
  * When applied on raw method, specifies that this raw method may be matched by many, arbitrarily named real methods.
  * In order to distinguish between real methods when translating raw call into real call,
  * multi raw method must take real method's RPC name (a `String`) as one of its parameters (see [[methodName]]).
  * By default, result type of multi raw method is [[encoded]] and the macro engine searches for
  * appropriate `AsRaw` or `AsReal` conversion between real method result type and raw method result type.
  *
  * When applied on raw parameter, specifies that this raw parameter may be matched by arbitrary number of real
  * parameters whose values are typically [[encoded]] and collected into (or extracted from) raw parameter value.
  * The way real parameter values are collected and extracted depends on the type of raw parameter which must be
  * either:
  *
  * - an `Iterable[R]` or any subtype (e.g. `List[R]`)
  * - a `PartialFunction[String,R]` or any subtype (e.g. `Map[String,R]`)
  *
  * `R` denotes the type used to represent each real parameter value. Be default (unless [[verbatim]] is used) it means
  * that each real value will be encoded as `R` and decoded from `R` - see [[encoded]] for more information about
  * how parameters are encoded and decoded.
  *
  * If raw parameter is a `Coll <: Iterable[Raw]` then in order to collect real values into raw value, the macro engine
  * will search for an instance of `CanBuildFrom[Nothing,R,Coll]` and use it to build the raw value.
  * In order to extract real values from raw value, macro generated code will obtain an `Iterator[R]` from the raw value
  * and pull and possibly decode all the values. If the iterator is exhausted before all real values could be obtained
  * then default parameter values specified on real parameters (if any) will be used as fallback values instead of
  * failing.
  *
  * If raw parameter is an `IndexedSeq[Raw]` then extraction is optimized - instead of using an iterator,
  * values are extracted by index (potentially also falling back to default values of real parameters).
  *
  * Finally, when raw parameter is a `Mapping <: PartialFunction[String,R]` then not only values but also parameter
  * names are collected - the macro engine will search for an instance of `CanBuildFrom[Nothing,(String,R),Mapping]`
  * in order to collect the mapping. Parameter names may be adjusted with [[rpcName]] but must be unique in the
  * scope of a single raw parameter. In order to extract real values from mapping, macro generated code will
  * call `applyOrElse` on the raw value giving it the name of each real parameter and also falling back to possible
  * default parameter values, in case they are missing in the raw call.
  *
  * Note that when raw parameter is a name-value mapping, you can freely reorder real parameter without fear of
  * breaking backwards compatibility. You can also safely add new real parameters as long as you provide default
  * values for them.
  *
  * Just like [[single]] and [[optional]], [[multi]] can also be applied on metadata parameters corresponding to
  * raw methods and raw parameters. The type of multi metadata parameter must be a collection, in the same way
  * as it's required for multi raw parameters. Metadata classes materialized for raw methods and raw parameters
  * must extend `TypedMetadata[T]` where `T` will be matched against each real method result type or each real
  * parameter type.
  *
  * Ultimately, [[multi]] may be specified on metadata parameter that reifies annotations from real trait/method/param
  * (see [[reifyAnnot]]). Such metadata parameter must be a collection: any subtype of `Iterable[A]` where `A` is the
  * type of annotation being reified. The macro will then reify all matching annotations from real symbol, including
  * inherited ones.
  */
final class multi extends SymbolArity

/**
  * Filter applied on raw methods or parameters which tells the macro engine that this raw method or parameter may
  * only match real methods or parameters annotated with at least one annotation of given type `A` (or any subtype).
  *
  * This is similar to [[tagged]] but simplier. Tagging lets you explicitly specify all possible tag types and provide
  * default/fallback tags when no tag is present.
  *
  * @tparam A type of annotation required to be present on real method or parameter
  */
final class annotated[A <: StaticAnnotation] extends RawSymAnnotation

/**
  * Raw parameters annotated as `@auxiliary` match real parameters without "consuming" them. This means that
  * real parameters matched by an auxiliary raw parameter must also be matched by some non-auxiliary raw parameter.
  * This way these real params will be matched to more than one raw parameter and effectively duplicated.
  * However, auxiliary raw param may use different encoding that the non-auxiliary one. This may be useful for
  * implementors of raw RPC traits.
  *
  * When extracting real parameter values from raw calls, auxiliary parameters are
  * completely ignored and only the matching non-auxiliary raw param value is used.
  */
final class auxiliary extends RawParamAnnotation

/**
  * Base trait for annotations applied on RPC metadata parameters which tell the macro engine how to materialize
  * their values based on real RPC trait, its methods or their parameters.
  */
trait MetadataParamStrategy extends StaticAnnotation

/**
  * When a metadata parameter is annotated as `@infer`, RPC macro engine will materialize that parameter by searching
  * for an implicit value of that parameter's type. `@infer` is the default strategy assumed for implicit parameters
  * of metadata classes, so using this annotation explicitly is only needed when you want an implicit search done
  * for non-implicit parameter. This may be useful if, e.g. you want an inferred parameter to be a case class field.
  *
  * NOTE: By default, implicit search for `@infer` parameter does NOT affect the decision about whether some real
  * method or real parameter matches a metadata parameter. For example, if an implicit for `@infer` parameter cannot be
  * found, you will only know about it *after* the metadata materializing macro has already been expanded.
  * This behaviour can be changed with [[checked]] annotation.
  */
final class infer extends MetadataParamStrategy

/**
  * `@adtParamMetadata` applied on metadata parameter of ADT (case class) metadata class indicates that this parameter
  * holds metadata for ADT parameter(s) (one, some or all, depending on [[RpcArity]], tagging, etc.).
  **/
final class adtParamMetadata extends MetadataParamStrategy

/**
  * `@adtCaseMetadata` applied on metadata parameter of ADT hierarchy (sealed trait) metadata class indicates that
  * this parameter holds metadata for its case classes (one, some or all, depending on [[RpcArity]], tagging, etc.).
  **/
final class adtCaseMetadata extends MetadataParamStrategy

/**
  * Metadata parameter annotated as `@reifyAnnot` is intended to hold annotation(s) that must or may be present on the real
  * RPC trait, method or parameter. `@reifyAnnot` parameters may have arity, which means that they may be annotated as
  * [[single]] (the default), [[optional]] or [[multi]]. Arity annotation determines what parameter type the macro
  * engine expects:
  *
  * - for [[single]], metadata parameter type must extend `StaticAnnotation` and an annotation of that type must be
  * present on the real symbol or compilation error will be raised
  * - for [[optional]], metadata parameter type must be an `Option`/`Opt`/etc. that wraps some `StaticAnnotation`.
  * If that annotation is present on the real symbol, it will be reified as metadata value.
  * - for [[multi]], metadata parameter type must be a subtype of `Iterable[StaticAnnotation]`, e.g. `List[SomeAnnot]`.
  * The macro will then reify all annotations of that particular type present on the real symbol as metadata value.
  *
  * NOTE: all annotations are inherited from super/overridden symbols, i.e.
  * - for RPC traits, annotations are inherited from all superclasses and supertraits
  * - for RPC methods, annotations are inherited from all overridden or implemented abstract methods
  * - for RPC parameters, annotations are inherited from all corresponding parameters (by index, not name) from
  * all methods overridden or implemented by method containing the parameter for which metadata is being reified.
  */
final class reifyAnnot extends MetadataParamStrategy

/**
  * Metadata parameter typed as `Boolean` can be annotated with `@isAnnotated[SomeAnnotation]`. Boolean value will then
  * hold information about whether RPC trait, method or parameter for which metadata is materialized is annotated with
  * `SomeAnnotation` (or any subtype) or not.
  */
final class isAnnotated[T <: StaticAnnotation] extends MetadataParamStrategy

/**
  * This annotation may only be applied on metadata parameters of type `String` and instructs the macro engine
  * to reify the name of real RPC trait/method/parameter. Depending on the value of `useRawName` flag, the macro
  * will either take into account or ignore potential [[rpcName]] annotation.
  */
final class reifyName(val useRawName: Boolean = false) extends MetadataParamStrategy

/**
  * Metadata parameter annotated with this annotation must be of type `ParamPosition` - a class that holds
  * parameter index information - see scaladoc for `ParamPosition` for more details.
  */
final class reifyPosition extends MetadataParamStrategy

/**
  * Metadata parameter annotated with this annotation must be of type `ParamFlags` - a class that holds
  * parameter flags information - see scaladoc for `ParamFlags` for more details.
  */
final class reifyFlags extends MetadataParamStrategy

/**
  * May be applied on metadata parameters with [[infer]] annotation (or just implicit metadata parameters -
  * they have [[infer]] strategy by default). Metadata parameter annotated as [[checked]] makes the implicit search
  * for that metadata parameter influence the decision about whether some metadata parameter matches real method or
  * param or not. Without [[checked]] annotation, when implicit search for metadata parameter fails, the macro engine
  * ignores that fact and error is only reported after macro is fully expanded.
  */
final class checked extends StaticAnnotation