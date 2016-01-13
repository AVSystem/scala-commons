package com.avsystem.commons
package serialization

import java.time.{Duration, Instant}

/**
  * Author: ghik
  * Created: 12/01/16.
  */
object JavaTimeCodecs {
  implicit val InstantCodec: GenCodec[Instant] = GenCodec.createNullSafe(
    _.readTimestamp().map(Instant.ofEpochMilli).get,
    (output, instant) => output.writeTimestamp(instant.toEpochMilli)
  )

  implicit val DurationCodec: GenCodec[Duration] = GenCodec.createNullSafe(
    _.readLong().map(Duration.ofMillis).get,
    (output, duration) => output.writeLong(duration.toMillis)
  )
}
