package com.avsystem.commons
package hocon

/**
  * Use this type in data deserialized from HOCON to in order to read size in bytes represented with
  * [[https://github.com/lightbend/config/blob/master/HOCON.md#size-in-bytes-format HOCON's nice representation]].
  */
final case class SizeInBytes(bytes: Long)
object SizeInBytes {
  final val Zero = SizeInBytes(0)
  final val `1KiB` = SizeInBytes(1024L)
  final val `1MiB` = SizeInBytes(1024 * 1024L)
}
