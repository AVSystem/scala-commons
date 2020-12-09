package com.avsystem.commons
package serialization

/**
  * May be used on members of objects, case classes or any types having companion object with case class
  * like `apply` and `unapply`/`unapplySeq` methods in order to add additional, generated fields to data serialized by
  * auto-materialized `GenCodec`s.
  *
  * {{{
  *   case class User(id: Long, login: String) {
  *     @generated def upperLogin: String = login.toUpperCase
  *   }
  *   object User {
  *     implicit val codec: GenCodec[User] = GenCodec.materialize[User]
  *   }
  * }}}
  *
  * This annotation may be applied on `val`s, `var`s and `def`s. When applied on a `def`, it must be either parameterless
  * (no parameter lists or empty parameter list) or accept only implicit parameters, provided that all the implicit values
  * are available in the scope where `GenCodec` is materialized (implicit values will be "baked in" the codec).
  *
  * NOTE: `@generated` annotation may be defined on any level of inheritance hierarchy - it will be inherited
  * from implemented and overridden members.
  */
class generated extends StaticAnnotation
