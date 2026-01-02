package com.avsystem.commons
package hocon

/** To read the size in bytes represented in
  * [[https://github.com/lightbend/config/blob/master/HOCON.md#size-in-bytes-format HOCON format]], use this type
  * together with [[com.avsystem.commons.hocon.SizeInBytesMarker]] when deserializing data from HOCON.
  *
  * @see
  *   [[com.avsystem.commons.hocon.HoconGenCodecs.SizeInBytesCodec]]
  */
final case class SizeInBytes(bytes: Long)
object SizeInBytes {
  final val Zero = SizeInBytes(0)
  final val `1KiB` = SizeInBytes(1024L)
  final val `1MiB` = SizeInBytes(1024 * 1024L)
}
