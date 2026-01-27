package com.avsystem.commons
package jiop

import java.time.Instant

import com.avsystem.commons.misc.Timestamp

trait JavaTimeInterop {
  extension (instant: Instant) {
    def truncateToTimestamp: Timestamp = Timestamp(instant.toEpochMilli)
    def truncateToJDate: JDate = new JDate(instant.toEpochMilli)
  }
}

object JavaTimeInterop extends JavaTimeInterop
