package com.avsystem.commons
package serialization

/**
  * Can be applied on case class fields (for `GenCodec` generation) and RPC parameters (for RPC interfaces)
  * collected into a map of raw values.
  *
  * Makes the parameter optional, meaning that its type must be an `Option`, `Opt`, `OptArg` or any other type
  * that has an instance of [[com.avsystem.commons.meta.OptionLike OptionLike]].
  * When the value is empty, the resulting JSON object or map of raw values simply lacks the entry
  * corresponding to this parameter.
  *
  * [[optionalParam]] is an alternative, better way of expressing optional params as opposed to usage of
  * [[whenAbsent]] with [[transientDefault]], i.e.
  *
  * {{{
  *   case class Stuff(@optionalParam optInt: Opt[Int])
  *   object Stuff extends HasGenCodec[Stuff]
  * }}}
  *
  * works effectively the same as
  *
  * {{{
  *   case class Stuff(@transientDefault @whenAbsent(Opt.Empty) optInt: Opt[Int])
  *   object Stuff extends HasGenCodec[Stuff]
  * }}}
  *
  * The difference is that when using `@whenAbsent` + `@transientDefault`, an instance of `GenCodec[Opt[Int]]`
  * is looked up by the macro engine. When using `@optionalParam`, an instance of `GenCodec[Int]` is looked up
  * directly (`Opt` is automatically unwrapped from implicit search).
  */
class optionalParam extends StaticAnnotation
