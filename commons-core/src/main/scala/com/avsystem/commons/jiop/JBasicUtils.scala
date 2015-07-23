package com.avsystem.commons
package jiop

import java.{lang => jl, math => jm, util => ju}

/**
 * Author: ghik
 * Created: 10/07/15.
 */
trait JBasicUtils {
  type JByte = jl.Byte
  type JShort = jl.Short
  type JInteger = jl.Integer
  type JLong = jl.Long
  type JFloat = jl.Float
  type JDouble = jl.Double
  type JBoolean = jl.Boolean
  type JCharacter = jl.Character
  type JBigInteger = jm.BigInteger
  type JBigDecimal = jm.BigDecimal
  type JDate = ju.Date
}
