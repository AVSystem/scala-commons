package com.avsystem.commons
package redis

import org.scalatest.{BeforeAndAfterEach, Suite}

/** Author: ghik Created: 29/07/16.
  */
trait CommunicationLogging extends BeforeAndAfterEach { this: Suite =>
  protected val listener = new TestDebugListener

  protected def assertCommunication(comm: String): Unit = {
    assert(listener.result().trim == comm.trim)
  }

  override protected def beforeEach() = {
    super.beforeEach()
    listener.clear()
  }
}
