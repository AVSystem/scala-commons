package com.avsystem.commons
package serialization

import com.avsystem.commons.meta.OptionLike

final class OptionalFieldValueCodec[O, V](optionLike: OptionLike.Aux[O, V], valueCodec: GenCodec[V]) extends GenCodec[O] {
  def read(input: Input): O =
    if (optionLike.ignoreNulls && input.readNull()) optionLike.none
    else optionLike.some(valueCodec.read(input))

  def write(output: Output, value: O): Unit =
    optionLike.fold(value, output.writeNull())(valueCodec.write(output, _))
}
