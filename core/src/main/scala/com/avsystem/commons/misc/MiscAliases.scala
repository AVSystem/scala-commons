package com.avsystem.commons
package misc

trait MiscAliases {
  type Opt[+A] = com.avsystem.commons.misc.Opt[A]
  final val Opt = com.avsystem.commons.misc.Opt
  type OptArg[+A] = com.avsystem.commons.misc.OptArg[A]
  final val OptArg = com.avsystem.commons.misc.OptArg
  type ImplicitOptArg[+A] = com.avsystem.commons.misc.ImplicitOptArg[A]
  final val ImplicitOptArg = com.avsystem.commons.misc.ImplicitOptArg
  type NOpt[+A] = com.avsystem.commons.misc.NOpt[A]
  final val NOpt = com.avsystem.commons.misc.NOpt
  type OptRef[+A >: Null] = com.avsystem.commons.misc.OptRef[A]
  final val OptRef = com.avsystem.commons.misc.OptRef
}
object MiscAliases extends MiscAliases
