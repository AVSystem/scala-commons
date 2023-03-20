package com.avsystem.commons
package meta

/**
  * Wrap your implicit instance of `AsReal`, `AsRaw`, `AsRawReal` or RPC metadata (with companion that extends
  * `RpcMetadataCompanion`) into Fallback` in order to lower its implicit priority.
  * Useful when some implicit must be imported but we don't want it to get higher priority that imports normally
  * have over implicit scope (e.g. implicits from companion objects).
  *
  * NOTE: `Fallback` does not work for *all* typeclasses, only RPC-related ones (`AsReal`, `AsRaw`, etc).
  * You can make it work with your own typeclass, but you must define appropriate forwarder in its companion, e.g.
  * {{{
  *   trait FallbackAwareTC[T] { ... }
  *   object FallbackAwareTC {
  *     implicit def fromFallback[T](implicit f: Fallback[FallbackAwareTC[T]]): FallbackAwareTC[T] = f.value
  *   }
  * }}}
  **/
case class Fallback[+T](value: T) extends AnyVal
