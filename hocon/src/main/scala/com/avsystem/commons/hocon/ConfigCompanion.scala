package com.avsystem.commons
package hocon

import com.avsystem.commons.meta.MacroInstances
import com.avsystem.commons.misc.ValueOf
import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec, GenObjectCodec, HasGenObjectCodecWithDeps}
import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.typesafe.config.{Config, ConfigFactory, ConfigObject}

import scala.concurrent.duration.*

trait CommonsHoconCodecs {
  implicit final val configCodec: GenCodec[Config] = GenCodec.nullable(
    input =>
      input.readCustom(ConfigValueMarker).map {
        case obj: ConfigObject => obj.toConfig
        case v => throw new ReadFailure(s"expected a config OBJECT, got ${v.valueType}")
      }.getOrElse {
        ConfigFactory.parseString(input.readSimple().readString())
      },
    (output, value) =>
      if (!output.writeCustom(ConfigValueMarker, value.root)) {
        output.writeSimple().writeString(value.root.render)
      },
  )

  implicit final val finiteDurationCodec: GenCodec[FiniteDuration] = GenCodec.nullable(
    input => input.readCustom(DurationMarker).fold(input.readSimple().readLong())(_.toMillis).millis,
    (output, value) => output.writeSimple().writeLong(value.toMillis),
  )

  implicit final val sizeInBytesCodec: GenCodec[SizeInBytes] = GenCodec.nonNull(
    input => SizeInBytes(input.readCustom(SizeInBytesMarker).getOrElse(input.readSimple().readLong())),
    (output, value) => output.writeSimple().writeLong(value.bytes),
  )

  implicit final val classKeyCodec: GenKeyCodec[Class[?]] =
    GenKeyCodec.create(Class.forName, _.getName)

  implicit final val classCodec: GenCodec[Class[?]] =
    GenCodec.nullableString(Class.forName, _.getName)
}
object CommonsHoconCodecs extends CommonsHoconCodecs

/**
  * Base class for companion objects of configuration case classes and sealed traits
  * (typically deserialized from HOCON files).
  *
  * [[ConfigCompanion]] is equivalent to [[com.avsystem.commons.serialization.HasGenCodec HasGenCodec]]
  * except that it automatically imports codecs from [[CommonsHoconCodecs]] - codecs for third party types often used
  * in configuration.
  */
abstract class ConfigCompanion[T](implicit
  macroCodec: MacroInstances[CommonsHoconCodecs.type, () => GenObjectCodec[T]],
) extends HasGenObjectCodecWithDeps[CommonsHoconCodecs.type, T] {
  final def read(config: Config): T = HoconInput.read[T](config)
}

/**
  * A version of [[ConfigCompanion]] which injects additional implicits into macro materialization.
  * Implicits are imported from an object specified with type parameter `D`.
  * It must be a singleton object type, i.e. `SomeObject.type`.
  */
abstract class ConfigCompanionWithDeps[T, D <: CommonsHoconCodecs](implicit
  deps: ValueOf[D],
  macroCodec: MacroInstances[D, () => GenObjectCodec[T]],
) extends HasGenObjectCodecWithDeps[D, T] {
  final def read(config: Config): T = HoconInput.read[T](config)
}
