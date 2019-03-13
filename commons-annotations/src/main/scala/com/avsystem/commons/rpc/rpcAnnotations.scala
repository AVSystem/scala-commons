package com.avsystem.commons
package rpc

import com.avsystem.commons.meta._

/**
  * You can use this annotation on overloaded RPC methods to give them unique identifiers for RPC serialization.
  * You can also subclass this annotation provided that you always override the `name` parameter with another
  * constructor parameter.
  */
class rpcName(val name: String) extends RealSymAnnotation

/**
  * You can use this annotation on real RPC methods to instruct macro engine to prepend method name (or [[rpcName]] if
  * specified) with given prefix. This annotation is mostly useful when aggregated by another annotation e.g.
  *
  * {{{
  *   sealed trait RestMethod extends RpcTag
  *   final class GET extends RestMethod with AnnotationAggregate {
  *     @rpcNamePrefix("GET_") type Implied
  *   }
  * }}}
  */
class rpcNamePrefix(val prefix: String, val overloadedOnly: Boolean = false) extends RealSymAnnotation

/**
  * Base trait for RPC tag annotations. Tagging gives more direct control over how real methods
  * and their parameters are matched against raw methods and their parameters.
  * For more information about method tagging, see documentation of [[methodTag]].
  * For more information about parameter tagging, see documentation of [[paramTag]].
  */
trait RpcTag extends RealSymAnnotation

/**
  * May be applied on raw method parameter of type `String` to indicate that macro generated implementation of
  * `AsReal` should pass real method's RPC name as this parameter and that macro generated implementation of
  * `AsRaw` should expect real method's RPC name to be passed there.
  *
  * Macro generation of `AsRaw` implementations require that raw methods annotated as
  * [[com.avsystem.commons.meta.multi multi]] must take at least
  * one raw parameter annotated as [[methodName]] (it may also be aggregated into some
  * [[com.avsystem.commons.meta.composite composite]] parameter).
  * This is necessary to properly identify which real method should be called.
  */
final class methodName extends RawParamAnnotation

/**
  * `@rpcMethodMetadata` applied on metadata parameter of RPC trait metadata class indicates that this parameter holds
  * metadata for RPC method(s) (one, some or all, depending on [[com.avsystem.commons.meta.SymbolArity SymbolArity]],
  * tagging, etc.).
  **/
final class rpcMethodMetadata extends MetadataParamStrategy

/**
  * `@rpcParamMetadata` applied on metadata parameter of RPC method metadata class indicates that this parameter holds
  * metadata for RPC parameter(s) (one, some or all, depending on [[com.avsystem.commons.meta.SymbolArity SymbolArity]]],
  * tagging, etc.).
  **/
final class rpcParamMetadata extends MetadataParamStrategy

/**
  * Base trait for [[verbatim]] and [[encoded]]. These annotations can be applied either on a raw method or
  * raw parameter in order to specify how matching real method results or matching real parameter values are encoded
  * as raw values.
  * Currently there are two possible cases: [[verbatim]] (no encoding) and [[encoded]] (encoding using `AsRaw` and
  * `AsReal` typeclasses). By default, method return values and [[com.avsystem.commons.meta.multi multi]]
  * parameters are [[encoded]] while [[com.avsystem.commons.meta.single single]] and
  * [[com.avsystem.commons.meta.optional optional]] parameters are [[verbatim]].
  * See documentation of [[verbatim]] and [[encoded]] for more details.
  */
sealed trait RpcEncoding extends RawMethodAnnotation with RawParamAnnotation

/**
  * When a raw parameter is annotated as [[encoded]], macro generated code will translate
  * between real parameter values and raw parameter values using implicit instances of `AsRaw[Raw,Real]`
  * and `AsReal[Raw,Real]` typeclasses. This annotation may also be applied on a method, but this would be
  * redundant since method results are encoded by default.
  *
  * Here's an example of raw RPC definition supposed to handle asynchronous (`Future`-based) calls that uses
  * `GenCodec` in order to encode and decode arguments and results as JSON strings.
  * It introduces its own wrapper class for JSON strings that has appropriate implicit instances of
  * `AsRaw` and `AsReal` (or `AsRawReal` which serves as both `AsReal` and `AsRaw`).
  *
  * {{{
  * import com.avsystem.commons._
  * import com.avsystem.commons.rpc._
  * import com.avsystem.commons.serialization._
  * import com.avsystem.commons.serialization.json._
  *
  * case class Json(jsonStr: String)
  * object Json {
  *   private def readJson[T: GenCodec](json: Json): T =
  *     JsonStringInput.read[T](json.jsonStr)
  *
  *   private def writeJson[T: GenCodec](value: T): Json =
  *     Json(JsonStringOutput.write[T](value))
  *
  *   implicit def genCodecBasedJsonEncoding[T: GenCodec]: AsRawReal[T,Json] =
  *     AsRawReal.create[Json,T](writeJson[T], readJson[T])
  *
  *   // instead of using `mapNow`, this method can also take implicit ExecutionContext and just use `map`
  *   implicit def genCodecBasedFutureJsonEncoding[T: GenCodec]: AsRawReal[Future[Json],Future[T]] =
  *     AsRawReal.create[Future[Json],Future[T]](_.mapNow(writeJson[T]), _.mapNow(readJson[T]))
  * }
  *
  * trait AsyncRawRpc {
  *   def call(@methodName rpcName: String, @multi args: Map[String,Json]): Future[Json]
  * }
  * }}}
  *
  * If you don't want to introduce the wrapper `Json` class and use more raw type, e.g. plain `String` then you
  * can also do it by moving implicit instances of `AsReal` and `AsRaw` (or the joined `AsRawReal`) into the
  * `implicits` object of raw RPC companion:
  *
  * {{{
  * trait AsyncRawRpc {
  *   def call(@methodName rpcName: String, @multi args: Map[String,String]): Future[String]
  * }
  * object AsyncRawRpc extends RawRpcCompanion[AsyncRawRpc] {
  *   private def readJson[T: GenCodec](json: String): T =
  *     JsonStringInput.read[T](json)
  *   private def writeJson[T: GenCodec](value: T): String =
  *     JsonStringOutput.write[T](value)
  *
  *   override object implicits {
  *     implicit def genCodecBasedJsonEncoding[T: GenCodec]: AsRawReal[String,T] =
  *       AsRawReal.create[String,T](writeJson[T], readJson[T])
  *     implicit def genCodecBasedFutureJsonEncoding[T: GenCodec]: AsRawReal[Future[String],Future[T]] =
  *       AsRawReal.create[Future[String],Future[T]](_.mapNow(writeJson[T]), _.mapNow(readJson[T]))
  *   }
  * }
  * }}}
  */
final class encoded extends RpcEncoding

/**
  * Turns off raw value encoding as specified by [[encoded]]. By default, [[com.avsystem.commons.meta.single single]]
  * and [[com.avsystem.commons.meta.optional optional]] raw parameters are already [[verbatim]], so using [[verbatim]]
  * only makes sense on [[com.avsystem.commons.meta.multi multi]] raw parameters or raw methods themselves,
  * which means turning off encoding of method's result.
  *
  * When encoding is turned off, raw and real types must be exactly the same types. For example, the following raw RPC
  * definition will match only raw RPC traits whose methods take `Int`s as parameters and return `Double`s as values:
  *
  * {{{
  * trait VerbatimRawRpc {
  *   @verbatim def call(@methodName rpcName: String, @multi @verbatim args: Map[String,Int]): Double
  * }
  * }}}
  */
final class verbatim extends RpcEncoding

/**
  * When raw method is annotated as `@tried`, invocations of real methods matching that raw method will be
  * automatically wrapped into `Try`. Consequently, all real methods will be treated as if their result
  * type was `Try[Result]` instead of actual `Result`. For example, if raw method is [[encoded]] and its
  * (raw) result is `Raw` then macro engine will search for implicit `AsRaw/Real[Raw,Try[Result]]` instead of just
  * `AsRaw/Real[Raw,Result]`
  */
final class tried extends RawMethodAnnotation

/**
  * When applied on raw RPC method or method metadata parameter, customizes error message displayed
  * for unmatched real method.
  *
  * When applied on raw RPC parameter or param metadata parameter, customizes error message displayed
  * when no real parameter matched annotated raw parameter. This implies that the raw parameter must have
  * [[single]] arity (otherwise it's not required to be matched by any real parameter).
  */
final class unmatched(error: String) extends RawSymAnnotation

/**
  * Can be applied on raw RPC method or method metadata parameter to customize compilation error message for
  * unmatched real parameters tagged as `Tag`.
  */
final class unmatchedParam[Tag <: RpcTag](error: String) extends RawMethodAnnotation

/**
  * Method tagging lets you have more explicit control over which raw methods can match which real methods.
  * Example:
  *
  * {{{
  * sealed trait MethodType extends RpcTag
  * class GET extends RestMethod
  * class POST extends RestMethod
  *
  * @methodTag[MethodType](new GET)
  * trait ExampleRawRpc {
  *   @tagged[GET] def get(@methodName name: String, @multi args: Map[String,Json]): Future[Json]
  *   @tagged[POST] def post(@methodName name: String, @multi args: Map[String,Json]): Future[Json]
  * }
  * }}}
  *
  * In the example above, we created a hierarchy of annotations rooted at `MethodType` which can be used
  * on real methods in order to explicitly tell the RPC macro which raw methods can match it.
  * We also specify `new GET` as the default tag that will be assumed for real methods without any tag annotation.
  * Then, using [[tagged]] we specify that the raw `get` method may only match real methods annotated as `GET`
  * while `post` raw method may only match real methods annotated as `POST`.
  * Raw methods not annotated with [[tagged]] have no limitations and may still match any real methods.
  *
  * Also, instead of specifying `defaultTag` in `@methodTag` annotation, you may provide the `whenUntagged`
  * parameter to [[tagged]] annotation. Raw method annotated as `@tagged[MethodType](whenUntagged = new GET)`
  * will match real methods either explicitly tagged with `GET` or untagged. If untagged, `new GET` will be assumed
  * as the tag. This is useful when you want to have multiple raw methods with different `whenUntagged` setting.
  *
  * NOTE: The example above assumes there is a Json` type defined with appropriate encodings -
  * see [[encoded]] for more details on parameter and method result encoding.
  *
  * An example of real RPC for `ExampleRawRpc`:
  *
  * {{{
  * trait ExampleApi {
  *   def getUser(id: UserId): Future[User]
  *   @POST def saveUser(user: User): Future[Unit]
  * }
  * object ExampleApi {
  *   implicit val AsRawReal: AsRawReal[ExampleRawRpc,ExampleApi] = AsRawReal.materialize
  * }
  * }}}
  *
  * @tparam BaseTag base type for tags that can be used on real RPC methods
  * @param defaultTag default tag value assumed for untagged methods
  **/
final class methodTag[BaseTag <: RpcTag](val defaultTag: BaseTag = null) extends RawRpcAnnotation

/**
  * Parameter tagging lets you have more explicit control over which raw parameters can match which real
  * parameters. This way you can have some of the parameters annotated in order to treat them differently, e.g.
  * they may be [[verbatim]], encoded in a different way or collected to a different raw container (e.g.
  * `Map[String,Raw]` vs `List[Raw]` - see [[com.avsystem.commons.meta.multi multi]] for more details).
  *
  * Example:
  * {{{
  * sealed trait RestParam extends RpcTag
  * class Body extends RestParam
  * class Url extends RestParam
  * class Path extends RestParam
  *
  * @paramTag[RestParam](new Body)
  * trait RestRawRpc {
  *   def get(
  *     @methodName name: String,
  *     @multi @verbatim @tagged[Path] pathParams: List[String],
  *     @multi @verbatim @tagged[Url] urlParams: Map[String,String],
  *     @multi @tagged[Body] bodyParams: Map[String,Json]
  *   ): Future[Json]
  * }
  * }}}
  *
  * NOTE: The example above assumes there is a `Json` type defined with appropriate encodings -
  * see [[encoded]] for more details on parameter and method result encoding.
  *
  * The example above configures parameter tag type for the entire trait, but you can also do it for
  * each raw method, e.g.
  *
  * {{{
  * trait RestRawRpc {
  *   @paramTag[RestParam](new Body)
  *   def get(...)
  * }
  * }}}
  *
  * @tparam BaseTag base type for tags that can be used on real RPC parameters
  * @param defaultTag default tag value assumed for untagged real parameters
  */
final class paramTag[BaseTag <: RpcTag](val defaultTag: BaseTag = null) extends RawMethodAnnotation

/**
  * Annotation applied on raw methods or raw parameters that limits matching real methods or real parameters to
  * only these annotated as `Tag`. See [[methodTag]] and [[paramTag]] for more explanation. NOTE: `Tag` may
  * also be some common supertype of multiple tags which are accepted by this raw method or param.
  *
  * @tparam Tag annotation type required to be present on real method or parameter
  * @param whenUntagged default tag value assumed for untagged methods/parameters - if specified, this effectively
  *                     means that raw method/parameter will also match untagged real methods/parameters and assume
  *                     the default tag value for them
  */
final class tagged[Tag <: RpcTag](val whenUntagged: Tag = null)
  extends RawMethodAnnotation with RawParamAnnotation
