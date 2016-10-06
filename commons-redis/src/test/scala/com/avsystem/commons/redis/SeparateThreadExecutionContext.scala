package com.avsystem.commons
package redis

import scala.concurrent.ExecutionContext

/**
  * Author: ghik
  * Created: 06/10/16.
  */
object SeparateThreadExecutionContext extends ExecutionContext {
  def execute(runnable: Runnable) = new Thread(runnable).start()
  def reportFailure(cause: Throwable) = cause.printStackTrace()
}
