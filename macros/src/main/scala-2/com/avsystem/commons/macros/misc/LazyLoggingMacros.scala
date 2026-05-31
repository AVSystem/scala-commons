package com.avsystem.commons
package macros.misc

import com.avsystem.commons.macros.MacroCommons

import scala.reflect.macros.blackbox

/** Author: ghik Created: 20/11/15.
  */
class LazyLoggingMacros(val c: blackbox.Context) extends MacroCommons {

  import c.universe._

  val DelegationCls = tq"$MiscPkg.Delegation"

  def warningImpl(msg: Tree) =
    q"""
      if(${c.prefix}.rawLog.isWarningEnabled) {
        ${c.prefix}.rawLog.warning($msg)
      }
     """

  def infoImpl(msg: Tree) =
    q"""
      if(${c.prefix}.rawLog.isInfoEnabled) {
        ${c.prefix}.rawLog.info($msg)
      }
     """

  def debugImpl(msg: Tree) =
    q"""
      if(${c.prefix}.rawLog.isDebugEnabled) {
        ${c.prefix}.rawLog.debug($msg)
      }
     """
}
