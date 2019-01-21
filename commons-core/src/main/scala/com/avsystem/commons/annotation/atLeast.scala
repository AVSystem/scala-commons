package com.avsystem.commons
package annotation

/**
  * When applied on varargs parameter, indicates that at least some number of parameters is required.
  * This is later checked by the static analyzer.
  * <br/>
  * WARNING: implementation of method which takes a varargs parameter may NOT assume that given number of
  * arguments will always be passed, because it's still possible to pass a `Seq` where
  * varargs parameter is required using the `: _*` ascription, e.g.
  * {{{
  *   varargsMethod(List(): _*)
  * }}}
  * and that is not checked by the static analyzer.
  */
class atLeast(n: Int) extends StaticAnnotation
