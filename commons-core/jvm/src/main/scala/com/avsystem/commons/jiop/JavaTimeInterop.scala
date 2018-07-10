package com.avsystem.commons
package jiop

import java.time.Instant

import com.avsystem.commons.misc.TimestampConversions

trait JavaTimeInterop {
  implicit def instantTimestampConversions(instant: Instant): TimestampConversions =
    new TimestampConversions(instant.toEpochMilli)
}
