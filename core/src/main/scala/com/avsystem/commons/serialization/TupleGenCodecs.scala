package com.avsystem.commons
package serialization

trait TupleGenCodecs { this: GenCodec.type =>
  // TODO[scala3-port]: mkTupleCodec (Scala 2 macro def) (L)
  private def mkTupleCodec[T](elementCodecs: GenCodec[?]*): GenCodec[T] = ???

  given tuple2Codec[T1, T2](using r1: GenCodec[T1], r2: GenCodec[T2]): GenCodec[(T1, T2)] =
    mkTupleCodec(r1, r2)

  given tuple3Codec[T1, T2, T3](using r1: GenCodec[T1], r2: GenCodec[T2], r3: GenCodec[T3]): GenCodec[(T1, T2, T3)] =
    mkTupleCodec(r1, r2, r3)

  given tuple4Codec[T1, T2, T3, T4](
    using r1: GenCodec[T1],
    r2: GenCodec[T2],
    r3: GenCodec[T3],
    r4: GenCodec[T4],
  ): GenCodec[(T1, T2, T3, T4)] =
    mkTupleCodec(r1, r2, r3, r4)

  given tuple5Codec[T1, T2, T3, T4, T5](
    using r1: GenCodec[T1],
    r2: GenCodec[T2],
    r3: GenCodec[T3],
    r4: GenCodec[T4],
    r5: GenCodec[T5],
  ): GenCodec[(T1, T2, T3, T4, T5)] =
    mkTupleCodec(r1, r2, r3, r4, r5)

  given tuple6Codec[T1, T2, T3, T4, T5, T6](
    using r1: GenCodec[T1],
    r2: GenCodec[T2],
    r3: GenCodec[T3],
    r4: GenCodec[T4],
    r5: GenCodec[T5],
    r6: GenCodec[T6],
  ): GenCodec[(T1, T2, T3, T4, T5, T6)] =
    mkTupleCodec(r1, r2, r3, r4, r5, r6)

  given tuple7Codec[T1, T2, T3, T4, T5, T6, T7](
    using r1: GenCodec[T1],
    r2: GenCodec[T2],
    r3: GenCodec[T3],
    r4: GenCodec[T4],
    r5: GenCodec[T5],
    r6: GenCodec[T6],
    r7: GenCodec[T7],
  ): GenCodec[(T1, T2, T3, T4, T5, T6, T7)] =
    mkTupleCodec(r1, r2, r3, r4, r5, r6, r7)

  given tuple8Codec[T1, T2, T3, T4, T5, T6, T7, T8](
    using r1: GenCodec[T1],
    r2: GenCodec[T2],
    r3: GenCodec[T3],
    r4: GenCodec[T4],
    r5: GenCodec[T5],
    r6: GenCodec[T6],
    r7: GenCodec[T7],
    r8: GenCodec[T8],
  ): GenCodec[(T1, T2, T3, T4, T5, T6, T7, T8)] =
    mkTupleCodec(r1, r2, r3, r4, r5, r6, r7, r8)

  given tuple9Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9](
    using r1: GenCodec[T1],
    r2: GenCodec[T2],
    r3: GenCodec[T3],
    r4: GenCodec[T4],
    r5: GenCodec[T5],
    r6: GenCodec[T6],
    r7: GenCodec[T7],
    r8: GenCodec[T8],
    r9: GenCodec[T9],
  ): GenCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9)] =
    mkTupleCodec(r1, r2, r3, r4, r5, r6, r7, r8, r9)

  given tuple10Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](
    using r1: GenCodec[T1],
    r2: GenCodec[T2],
    r3: GenCodec[T3],
    r4: GenCodec[T4],
    r5: GenCodec[T5],
    r6: GenCodec[T6],
    r7: GenCodec[T7],
    r8: GenCodec[T8],
    r9: GenCodec[T9],
    r10: GenCodec[T10],
  ): GenCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10)] =
    mkTupleCodec(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10)

  given tuple11Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](
    using r1: GenCodec[T1],
    r2: GenCodec[T2],
    r3: GenCodec[T3],
    r4: GenCodec[T4],
    r5: GenCodec[T5],
    r6: GenCodec[T6],
    r7: GenCodec[T7],
    r8: GenCodec[T8],
    r9: GenCodec[T9],
    r10: GenCodec[T10],
    r11: GenCodec[T11],
  ): GenCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11)] =
    mkTupleCodec(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11)

  given tuple12Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](
    using r1: GenCodec[T1],
    r2: GenCodec[T2],
    r3: GenCodec[T3],
    r4: GenCodec[T4],
    r5: GenCodec[T5],
    r6: GenCodec[T6],
    r7: GenCodec[T7],
    r8: GenCodec[T8],
    r9: GenCodec[T9],
    r10: GenCodec[T10],
    r11: GenCodec[T11],
    r12: GenCodec[T12],
  ): GenCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12)] =
    mkTupleCodec(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12)

  given tuple13Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](
    using r1: GenCodec[T1],
    r2: GenCodec[T2],
    r3: GenCodec[T3],
    r4: GenCodec[T4],
    r5: GenCodec[T5],
    r6: GenCodec[T6],
    r7: GenCodec[T7],
    r8: GenCodec[T8],
    r9: GenCodec[T9],
    r10: GenCodec[T10],
    r11: GenCodec[T11],
    r12: GenCodec[T12],
    r13: GenCodec[T13],
  ): GenCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13)] =
    mkTupleCodec(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13)

  given tuple14Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](
    using r1: GenCodec[T1],
    r2: GenCodec[T2],
    r3: GenCodec[T3],
    r4: GenCodec[T4],
    r5: GenCodec[T5],
    r6: GenCodec[T6],
    r7: GenCodec[T7],
    r8: GenCodec[T8],
    r9: GenCodec[T9],
    r10: GenCodec[T10],
    r11: GenCodec[T11],
    r12: GenCodec[T12],
    r13: GenCodec[T13],
    r14: GenCodec[T14],
  ): GenCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14)] =
    mkTupleCodec(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14)

  given tuple15Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](
    using r1: GenCodec[T1],
    r2: GenCodec[T2],
    r3: GenCodec[T3],
    r4: GenCodec[T4],
    r5: GenCodec[T5],
    r6: GenCodec[T6],
    r7: GenCodec[T7],
    r8: GenCodec[T8],
    r9: GenCodec[T9],
    r10: GenCodec[T10],
    r11: GenCodec[T11],
    r12: GenCodec[T12],
    r13: GenCodec[T13],
    r14: GenCodec[T14],
    r15: GenCodec[T15],
  ): GenCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15)] =
    mkTupleCodec(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15)

  given tuple16Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](
    using r1: GenCodec[T1],
    r2: GenCodec[T2],
    r3: GenCodec[T3],
    r4: GenCodec[T4],
    r5: GenCodec[T5],
    r6: GenCodec[T6],
    r7: GenCodec[T7],
    r8: GenCodec[T8],
    r9: GenCodec[T9],
    r10: GenCodec[T10],
    r11: GenCodec[T11],
    r12: GenCodec[T12],
    r13: GenCodec[T13],
    r14: GenCodec[T14],
    r15: GenCodec[T15],
    r16: GenCodec[T16],
  ): GenCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16)] =
    mkTupleCodec(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16)

  given tuple17Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](
    using r1: GenCodec[T1],
    r2: GenCodec[T2],
    r3: GenCodec[T3],
    r4: GenCodec[T4],
    r5: GenCodec[T5],
    r6: GenCodec[T6],
    r7: GenCodec[T7],
    r8: GenCodec[T8],
    r9: GenCodec[T9],
    r10: GenCodec[T10],
    r11: GenCodec[T11],
    r12: GenCodec[T12],
    r13: GenCodec[T13],
    r14: GenCodec[T14],
    r15: GenCodec[T15],
    r16: GenCodec[T16],
    r17: GenCodec[T17],
  ): GenCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17)] =
    mkTupleCodec(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17)

  given tuple18Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](
    using r1: GenCodec[T1],
    r2: GenCodec[T2],
    r3: GenCodec[T3],
    r4: GenCodec[T4],
    r5: GenCodec[T5],
    r6: GenCodec[T6],
    r7: GenCodec[T7],
    r8: GenCodec[T8],
    r9: GenCodec[T9],
    r10: GenCodec[T10],
    r11: GenCodec[T11],
    r12: GenCodec[T12],
    r13: GenCodec[T13],
    r14: GenCodec[T14],
    r15: GenCodec[T15],
    r16: GenCodec[T16],
    r17: GenCodec[T17],
    r18: GenCodec[T18],
  ): GenCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18)] =
    mkTupleCodec(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18)

  given tuple19Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](
    using r1: GenCodec[T1],
    r2: GenCodec[T2],
    r3: GenCodec[T3],
    r4: GenCodec[T4],
    r5: GenCodec[T5],
    r6: GenCodec[T6],
    r7: GenCodec[T7],
    r8: GenCodec[T8],
    r9: GenCodec[T9],
    r10: GenCodec[T10],
    r11: GenCodec[T11],
    r12: GenCodec[T12],
    r13: GenCodec[T13],
    r14: GenCodec[T14],
    r15: GenCodec[T15],
    r16: GenCodec[T16],
    r17: GenCodec[T17],
    r18: GenCodec[T18],
    r19: GenCodec[T19],
  ): GenCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19)] =
    mkTupleCodec(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19)

  given tuple20Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](
    using r1: GenCodec[T1],
    r2: GenCodec[T2],
    r3: GenCodec[T3],
    r4: GenCodec[T4],
    r5: GenCodec[T5],
    r6: GenCodec[T6],
    r7: GenCodec[T7],
    r8: GenCodec[T8],
    r9: GenCodec[T9],
    r10: GenCodec[T10],
    r11: GenCodec[T11],
    r12: GenCodec[T12],
    r13: GenCodec[T13],
    r14: GenCodec[T14],
    r15: GenCodec[T15],
    r16: GenCodec[T16],
    r17: GenCodec[T17],
    r18: GenCodec[T18],
    r19: GenCodec[T19],
    r20: GenCodec[T20],
  ): GenCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20)] =
    mkTupleCodec(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20)

  given tuple21Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](
    using r1: GenCodec[T1],
    r2: GenCodec[T2],
    r3: GenCodec[T3],
    r4: GenCodec[T4],
    r5: GenCodec[T5],
    r6: GenCodec[T6],
    r7: GenCodec[T7],
    r8: GenCodec[T8],
    r9: GenCodec[T9],
    r10: GenCodec[T10],
    r11: GenCodec[T11],
    r12: GenCodec[T12],
    r13: GenCodec[T13],
    r14: GenCodec[T14],
    r15: GenCodec[T15],
    r16: GenCodec[T16],
    r17: GenCodec[T17],
    r18: GenCodec[T18],
    r19: GenCodec[T19],
    r20: GenCodec[T20],
    r21: GenCodec[T21],
  ): GenCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21)] =
    mkTupleCodec(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21)

  given tuple22Codec[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](
    using r1: GenCodec[T1],
    r2: GenCodec[T2],
    r3: GenCodec[T3],
    r4: GenCodec[T4],
    r5: GenCodec[T5],
    r6: GenCodec[T6],
    r7: GenCodec[T7],
    r8: GenCodec[T8],
    r9: GenCodec[T9],
    r10: GenCodec[T10],
    r11: GenCodec[T11],
    r12: GenCodec[T12],
    r13: GenCodec[T13],
    r14: GenCodec[T14],
    r15: GenCodec[T15],
    r16: GenCodec[T16],
    r17: GenCodec[T17],
    r18: GenCodec[T18],
    r19: GenCodec[T19],
    r20: GenCodec[T20],
    r21: GenCodec[T21],
    r22: GenCodec[T22],
  ): GenCodec[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22)] =
    mkTupleCodec(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22)

}

object GenTupleDBCodecs {
  def main(args: Array[String]): Unit = {
    for (i <- 2 to 22) {
      val types = (1 to i).map(j => s"T$j")

      val tupleType = types.mkString("(", ",", ")")
      val implicits = (1 to i).map(j => s"r$j: DBCodec[T$j]").mkString("implicit\n", ",\n", "")

      println(
        s"""
           |implicit def tuple${i}Codec[${types.mkString(",")}]($implicits): DBCodec[$tupleType] =
           |  mkTupleCodec(${(1 to i).map(j => s"r$j").mkString(",")})
        """.stripMargin
      )
    }
  }
}
