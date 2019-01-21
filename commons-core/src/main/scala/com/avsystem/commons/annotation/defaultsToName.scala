package com.avsystem.commons
package annotation

/**
  * Meta annotation that may be used on `String` constructor parameter of an annotation. This constructor parameter
  * must take a default `null` value. [[defaultsToName]] makes annotation processing macro engines insert the name
  * of annotated symbol instead of `null`.
  *
  * @example
  *
  * {{{
  *   class SomeMethodAnnotation(@defaultsToName val name: String = null)
  *
  *   @SomeMethodAnnotation def someMethod: String
  * }}}
  *
  * Now, when some macro engine has to inspect `SomeMethodAnnotation` of `someMethod`, it will automatically insert
  * the string "someMethod" as the argument of `SomeMethodAnnotation`.
  */
class defaultsToName extends StaticAnnotation
