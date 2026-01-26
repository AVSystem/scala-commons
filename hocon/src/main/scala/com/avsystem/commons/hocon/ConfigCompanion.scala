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
  implicit def configCodec: GenCodec[Config] = HoconGenCodecs.ConfigCodec
  implicit def finiteDurationCodec: GenCodec[FiniteDuration] = HoconGenCodecs.FiniteDurationCodec
  implicit def jDurationCodec: GenCodec[JDuration] = HoconGenCodecs.JavaDurationCodec
  implicit def periodCodec: GenCodec[Period] = HoconGenCodecs.PeriodCodec
  implicit def sizeInBytesCodec: GenCodec[SizeInBytes] = HoconGenCodecs.SizeInBytesCodec
  implicit def classKeyCodec: GenKeyCodec[Class[?]] = HoconGenCodecs.ClassKeyCodec
  implicit def classCodec: GenCodec[Class[?]] = HoconGenCodecs.ClassCodec
}
object HoconGenCodecs {
  implicit final val ConfigCodec: GenCodec[Config] = GenCodec.nullable(
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

  implicit final val FiniteDurationCodec: GenCodec[FiniteDuration] = GenCodec.nullable(
    input =>
      input.readCustom(DurationMarker).map(DurationConverters.toScala).getOrElse(input.readSimple().readLong().millis),
    (output, value) =>
      if (!output.writeCustom(DurationMarker, DurationConverters.toJava(value)))
        output
          .writeSimple()
          .writeLong(value.toMillis),
  )

  implicit final val JavaDurationCodec: GenCodec[JDuration] = GenCodec.nullable(
    input => input.readCustom(DurationMarker).getOrElse(JDuration.ofMillis(input.readSimple().readLong())),
    (output, value) => if (!output.writeCustom(DurationMarker, value)) output.writeSimple().writeLong(value.toMillis),
  )

  implicit final val PeriodCodec: GenCodec[Period] = GenCodec.nullable(
    input => input.readCustom(PeriodMarker).getOrElse(Period.parse(input.readSimple().readString())),
    (output, value) => if (!output.writeCustom(PeriodMarker, value)) output.writeSimple().writeString(value.toString),
  )

  implicit final val SizeInBytesCodec: GenCodec[SizeInBytes] = GenCodec.nonNull(
    input => SizeInBytes(input.readCustom(SizeInBytesMarker).getOrElse(input.readSimple().readLong())),
    (output, value) =>
      if (!output.writeCustom(SizeInBytesMarker, value.bytes)) output.writeSimple().writeLong(value.bytes),
  )

  implicit final val ClassKeyCodec: GenKeyCodec[Class[?]] =
    GenKeyCodec.create(Class.forName, _.getName)

  implicit final val ClassCodec: GenCodec[Class[?]] =
    GenCodec.nullableString(Class.forName, _.getName)
}

object DefaultHoconGenCodecs extends HoconGenCodecs

trait ConfigObjectCodec[T] {
  def objectCodec: GenObjectCodec[T]
}

abstract class AbstractConfigCompanion[Implicits <: HoconGenCodecs, T](
  implicits: Implicits,
)(implicit instances: MacroInstances[Implicits, ConfigObjectCodec[T]],
) {
  implicit lazy val codec: GenCodec[T] = instances(implicits, this).objectCodec

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
abstract class DefaultConfigCompanion[T](implicit macroCodec: MacroInstances[HoconGenCodecs, ConfigObjectCodec[T]])
  extends AbstractConfigCompanion[HoconGenCodecs, T](DefaultHoconGenCodecs)
