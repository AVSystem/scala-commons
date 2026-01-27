package com.avsystem.commons
package hocon

import com.avsystem.commons.meta.MacroInstances
import com.avsystem.commons.serialization.GenCodec.ReadFailure
import com.avsystem.commons.serialization.{GenCodec, GenKeyCodec, GenObjectCodec}
import com.typesafe.config.{Config, ConfigFactory, ConfigObject, ConfigRenderOptions}

import java.time.{Duration as JDuration, Period}
import scala.concurrent.duration.*
import scala.jdk.javaapi.DurationConverters

trait HoconGenCodecs {
  given GenCodec[Config] = GenCodec.nullable(
    input =>
      input.readCustom(ConfigValueMarker).fold(ConfigFactory.parseString(input.readSimple().readString())) {
        case obj: ConfigObject => obj.toConfig
        case v => throw new ReadFailure(s"expected a config OBJECT, got ${v.valueType}")
      },
    (output, value) =>
      if (!output.writeCustom(ConfigValueMarker, value.root)) {
        val renderOptions = ConfigRenderOptions.defaults().setOriginComments(false)
        output.writeSimple().writeString(value.root.render(renderOptions))
      },
  )

  given GenCodec[FiniteDuration] = GenCodec.nullable(
    input =>
      input.readCustom(DurationMarker).map(DurationConverters.toScala).getOrElse(input.readSimple().readLong().millis),
    (output, value) =>
      if (!output.writeCustom(DurationMarker, DurationConverters.toJava(value)))
        output
          .writeSimple()
          .writeLong(value.toMillis),
  )

  given GenCodec[JDuration] = GenCodec.nullable(
    input => input.readCustom(DurationMarker).getOrElse(JDuration.ofMillis(input.readSimple().readLong())),
    (output, value) => if (!output.writeCustom(DurationMarker, value)) output.writeSimple().writeLong(value.toMillis),
  )

  given GenCodec[Period] = GenCodec.nullable(
    input => input.readCustom(PeriodMarker).getOrElse(Period.parse(input.readSimple().readString())),
    (output, value) => if (!output.writeCustom(PeriodMarker, value)) output.writeSimple().writeString(value.toString),
  )

  given GenCodec[SizeInBytes] = GenCodec.nonNull(
    input => SizeInBytes(input.readCustom(SizeInBytesMarker).getOrElse(input.readSimple().readLong())),
    (output, value) =>
      if (!output.writeCustom(SizeInBytesMarker, value.bytes)) output.writeSimple().writeLong(value.bytes),
  )

  given GenKeyCodec[Class[?]] =
    GenKeyCodec.create(Class.forName, _.getName)

  given GenCodec[Class[?]] =
    GenCodec.nullableString(Class.forName, _.getName)
}
object HoconGenCodecs extends HoconGenCodecs

object DefaultHoconGenCodecs extends HoconGenCodecs

trait ConfigObjectCodec[T] {
  def objectCodec: GenObjectCodec[T]
}

abstract class AbstractConfigCompanion[Implicits <: HoconGenCodecs, T](
  implicits: Implicits,
)(using instances: MacroInstances[Implicits, ConfigObjectCodec[T]],
) {
  given GenCodec[T] = instances(implicits, this).objectCodec

  final def read(config: Config): T = HoconInput.read[T](config)
}

/**
 * Base class for companion objects of configuration case classes and sealed traits (typically deserialized from HOCON
 * files).
 *
 * [[DefaultConfigCompanion]] is equivalent to [[com.avsystem.commons.serialization.HasGenCodec HasGenCodec]] except
 * that it automatically imports codecs from [[HoconGenCodecs]] - codecs for third party types often used in
 * configuration.
 */
abstract class DefaultConfigCompanion[T](using macroCodec: MacroInstances[HoconGenCodecs, ConfigObjectCodec[T]])
  extends AbstractConfigCompanion[HoconGenCodecs, T](DefaultHoconGenCodecs)
