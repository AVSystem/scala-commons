package com.avsystem.commons
package jiop

import java.time.Instant

import com.avsystem.commons.jiop.JavaTimeInterop.InstantOps

trait JavaTimeInterop {
  implicit def instantOps(instant: Instant): InstantOps = new InstantOps(instant)
}
object JavaTimeInterop {
  class InstantOps(private val instant: Instant) extends AnyVal {
    def truncateToTimestamp: Timestamp = Timestamp(instant.toEpochMilli)
    def truncateToJDate: JDate = new JDate(instant.toEpochMilli)
  }
}
