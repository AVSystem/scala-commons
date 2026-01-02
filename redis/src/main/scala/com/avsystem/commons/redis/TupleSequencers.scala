package com.avsystem.commons
package redis

import com.avsystem.commons.redis.RedisBatch.Index
import com.avsystem.commons.redis.TupleSequencers.TupleBatch
import com.avsystem.commons.redis.protocol.RedisReply

trait TupleSequencers { this: Sequencer.type =>
  implicit def tuple1Sequencer[O1, R1](implicit s1: Sequencer[O1, R1]): Sequencer[Tuple1[O1], Tuple1[R1]] =

    ops => s1.sequence(ops._1).map(Tuple1(_))

  implicit def tuple2Sequencer[O1, O2, R1, R2](implicit s1: Sequencer[O1, R1], s2: Sequencer[O2, R2])
    : Sequencer[(O1, O2), (R1, R2)] =

    ops =>
      new TupleBatch((s1.sequence(ops._1), s2.sequence(ops._2)), arr => (arr(0).asInstanceOf[R1], arr(1).asInstanceOf[R2]))

  implicit def tuple3Sequencer[O1, O2, O3, R1, R2, R3](
    implicit s1: Sequencer[O1, R1],
    s2: Sequencer[O2, R2],
    s3: Sequencer[O3, R3],
  ): Sequencer[(O1, O2, O3), (R1, R2, R3)] =

    ops =>
      new TupleBatch(
        (s1.sequence(ops._1), s2.sequence(ops._2), s3.sequence(ops._3)),
        arr => (arr(0).asInstanceOf[R1], arr(1).asInstanceOf[R2], arr(2).asInstanceOf[R3]),
      )

  implicit def tuple4Sequencer[O1, O2, O3, O4, R1, R2, R3, R4](
    implicit s1: Sequencer[O1, R1],
    s2: Sequencer[O2, R2],
    s3: Sequencer[O3, R3],
    s4: Sequencer[O4, R4],
  ): Sequencer[(O1, O2, O3, O4), (R1, R2, R3, R4)] =

    ops =>
      new TupleBatch(
        (s1.sequence(ops._1), s2.sequence(ops._2), s3.sequence(ops._3), s4.sequence(ops._4)),
        arr => (arr(0).asInstanceOf[R1], arr(1).asInstanceOf[R2], arr(2).asInstanceOf[R3], arr(3).asInstanceOf[R4]),
      )

  implicit def tuple5Sequencer[O1, O2, O3, O4, O5, R1, R2, R3, R4, R5](
    implicit s1: Sequencer[O1, R1],
    s2: Sequencer[O2, R2],
    s3: Sequencer[O3, R3],
    s4: Sequencer[O4, R4],
    s5: Sequencer[O5, R5],
  ): Sequencer[(O1, O2, O3, O4, O5), (R1, R2, R3, R4, R5)] =

    ops =>
      new TupleBatch(
        (s1.sequence(ops._1), s2.sequence(ops._2), s3.sequence(ops._3), s4.sequence(ops._4), s5.sequence(ops._5)),
        arr =>
          (
            arr(0).asInstanceOf[R1],
            arr(1).asInstanceOf[R2],
            arr(2).asInstanceOf[R3],
            arr(3).asInstanceOf[R4],
            arr(4).asInstanceOf[R5],
          ),
      )

  implicit def tuple6Sequencer[O1, O2, O3, O4, O5, O6, R1, R2, R3, R4, R5, R6](
    implicit s1: Sequencer[O1, R1],
    s2: Sequencer[O2, R2],
    s3: Sequencer[O3, R3],
    s4: Sequencer[O4, R4],
    s5: Sequencer[O5, R5],
    s6: Sequencer[O6, R6],
  ): Sequencer[(O1, O2, O3, O4, O5, O6), (R1, R2, R3, R4, R5, R6)] =

    ops =>
      new TupleBatch(
        (
          s1.sequence(ops._1),
          s2.sequence(ops._2),
          s3.sequence(ops._3),
          s4.sequence(ops._4),
          s5.sequence(ops._5),
          s6.sequence(ops._6),
        ),
        arr =>
          (
            arr(0).asInstanceOf[R1],
            arr(1).asInstanceOf[R2],
            arr(2).asInstanceOf[R3],
            arr(3).asInstanceOf[R4],
            arr(4).asInstanceOf[R5],
            arr(5).asInstanceOf[R6],
          ),
      )

  implicit def tuple7Sequencer[O1, O2, O3, O4, O5, O6, O7, R1, R2, R3, R4, R5, R6, R7](
    implicit s1: Sequencer[O1, R1],
    s2: Sequencer[O2, R2],
    s3: Sequencer[O3, R3],
    s4: Sequencer[O4, R4],
    s5: Sequencer[O5, R5],
    s6: Sequencer[O6, R6],
    s7: Sequencer[O7, R7],
  ): Sequencer[(O1, O2, O3, O4, O5, O6, O7), (R1, R2, R3, R4, R5, R6, R7)] =

    ops =>
      new TupleBatch(
        (
          s1.sequence(ops._1),
          s2.sequence(ops._2),
          s3.sequence(ops._3),
          s4.sequence(ops._4),
          s5.sequence(ops._5),
          s6.sequence(ops._6),
          s7.sequence(ops._7),
        ),
        arr =>
          (
            arr(0).asInstanceOf[R1],
            arr(1).asInstanceOf[R2],
            arr(2).asInstanceOf[R3],
            arr(3).asInstanceOf[R4],
            arr(4).asInstanceOf[R5],
            arr(5).asInstanceOf[R6],
            arr(6).asInstanceOf[R7],
          ),
      )

  implicit def tuple8Sequencer[O1, O2, O3, O4, O5, O6, O7, O8, R1, R2, R3, R4, R5, R6, R7, R8](
    implicit s1: Sequencer[O1, R1],
    s2: Sequencer[O2, R2],
    s3: Sequencer[O3, R3],
    s4: Sequencer[O4, R4],
    s5: Sequencer[O5, R5],
    s6: Sequencer[O6, R6],
    s7: Sequencer[O7, R7],
    s8: Sequencer[O8, R8],
  ): Sequencer[(O1, O2, O3, O4, O5, O6, O7, O8), (R1, R2, R3, R4, R5, R6, R7, R8)] =

    ops =>
      new TupleBatch(
        (
          s1.sequence(ops._1),
          s2.sequence(ops._2),
          s3.sequence(ops._3),
          s4.sequence(ops._4),
          s5.sequence(ops._5),
          s6.sequence(ops._6),
          s7.sequence(ops._7),
          s8.sequence(ops._8),
        ),
        arr =>
          (
            arr(0).asInstanceOf[R1],
            arr(1).asInstanceOf[R2],
            arr(2).asInstanceOf[R3],
            arr(3).asInstanceOf[R4],
            arr(4).asInstanceOf[R5],
            arr(5).asInstanceOf[R6],
            arr(6).asInstanceOf[R7],
            arr(7).asInstanceOf[R8],
          ),
      )

  implicit def tuple9Sequencer[O1, O2, O3, O4, O5, O6, O7, O8, O9, R1, R2, R3, R4, R5, R6, R7, R8, R9](
    implicit s1: Sequencer[O1, R1],
    s2: Sequencer[O2, R2],
    s3: Sequencer[O3, R3],
    s4: Sequencer[O4, R4],
    s5: Sequencer[O5, R5],
    s6: Sequencer[O6, R6],
    s7: Sequencer[O7, R7],
    s8: Sequencer[O8, R8],
    s9: Sequencer[O9, R9],
  ): Sequencer[(O1, O2, O3, O4, O5, O6, O7, O8, O9), (R1, R2, R3, R4, R5, R6, R7, R8, R9)] =

    ops =>
      new TupleBatch(
        (
          s1.sequence(ops._1),
          s2.sequence(ops._2),
          s3.sequence(ops._3),
          s4.sequence(ops._4),
          s5.sequence(ops._5),
          s6.sequence(ops._6),
          s7.sequence(ops._7),
          s8.sequence(ops._8),
          s9.sequence(ops._9),
        ),
        arr =>
          (
            arr(0).asInstanceOf[R1],
            arr(1).asInstanceOf[R2],
            arr(2).asInstanceOf[R3],
            arr(3).asInstanceOf[R4],
            arr(4).asInstanceOf[R5],
            arr(5).asInstanceOf[R6],
            arr(6).asInstanceOf[R7],
            arr(7).asInstanceOf[R8],
            arr(8).asInstanceOf[R9],
          ),
      )

  implicit def tuple10Sequencer[O1, O2, O3, O4, O5, O6, O7, O8, O9, O10, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10](
    implicit s1: Sequencer[O1, R1],
    s2: Sequencer[O2, R2],
    s3: Sequencer[O3, R3],
    s4: Sequencer[O4, R4],
    s5: Sequencer[O5, R5],
    s6: Sequencer[O6, R6],
    s7: Sequencer[O7, R7],
    s8: Sequencer[O8, R8],
    s9: Sequencer[O9, R9],
    s10: Sequencer[O10, R10],
  ): Sequencer[(O1, O2, O3, O4, O5, O6, O7, O8, O9, O10), (R1, R2, R3, R4, R5, R6, R7, R8, R9, R10)] =

    ops =>
      new TupleBatch(
        (
          s1.sequence(ops._1),
          s2.sequence(ops._2),
          s3.sequence(ops._3),
          s4.sequence(ops._4),
          s5.sequence(ops._5),
          s6.sequence(ops._6),
          s7.sequence(ops._7),
          s8.sequence(ops._8),
          s9.sequence(ops._9),
          s10.sequence(ops._10),
        ),
        arr =>
          (
            arr(0).asInstanceOf[R1],
            arr(1).asInstanceOf[R2],
            arr(2).asInstanceOf[R3],
            arr(3).asInstanceOf[R4],
            arr(4).asInstanceOf[R5],
            arr(5).asInstanceOf[R6],
            arr(6).asInstanceOf[R7],
            arr(7).asInstanceOf[R8],
            arr(8).asInstanceOf[R9],
            arr(9).asInstanceOf[R10],
          ),
      )

  implicit def tuple11Sequencer[
    O1,
    O2,
    O3,
    O4,
    O5,
    O6,
    O7,
    O8,
    O9,
    O10,
    O11,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
  ](implicit
    s1: Sequencer[O1, R1],
    s2: Sequencer[O2, R2],
    s3: Sequencer[O3, R3],
    s4: Sequencer[O4, R4],
    s5: Sequencer[O5, R5],
    s6: Sequencer[O6, R6],
    s7: Sequencer[O7, R7],
    s8: Sequencer[O8, R8],
    s9: Sequencer[O9, R9],
    s10: Sequencer[O10, R10],
    s11: Sequencer[O11, R11],
  ): Sequencer[(O1, O2, O3, O4, O5, O6, O7, O8, O9, O10, O11), (R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11)] =

    ops =>
      new TupleBatch(
        (
          s1.sequence(ops._1),
          s2.sequence(ops._2),
          s3.sequence(ops._3),
          s4.sequence(ops._4),
          s5.sequence(ops._5),
          s6.sequence(ops._6),
          s7.sequence(ops._7),
          s8.sequence(ops._8),
          s9.sequence(ops._9),
          s10.sequence(ops._10),
          s11.sequence(ops._11),
        ),
        arr =>
          (
            arr(0).asInstanceOf[R1],
            arr(1).asInstanceOf[R2],
            arr(2).asInstanceOf[R3],
            arr(3).asInstanceOf[R4],
            arr(4).asInstanceOf[R5],
            arr(5).asInstanceOf[R6],
            arr(6).asInstanceOf[R7],
            arr(7).asInstanceOf[R8],
            arr(8).asInstanceOf[R9],
            arr(9).asInstanceOf[R10],
            arr(10).asInstanceOf[R11],
          ),
      )

  implicit def tuple12Sequencer[
    O1,
    O2,
    O3,
    O4,
    O5,
    O6,
    O7,
    O8,
    O9,
    O10,
    O11,
    O12,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
  ](implicit
    s1: Sequencer[O1, R1],
    s2: Sequencer[O2, R2],
    s3: Sequencer[O3, R3],
    s4: Sequencer[O4, R4],
    s5: Sequencer[O5, R5],
    s6: Sequencer[O6, R6],
    s7: Sequencer[O7, R7],
    s8: Sequencer[O8, R8],
    s9: Sequencer[O9, R9],
    s10: Sequencer[O10, R10],
    s11: Sequencer[O11, R11],
    s12: Sequencer[O12, R12],
  ): Sequencer[
    (O1, O2, O3, O4, O5, O6, O7, O8, O9, O10, O11, O12),
    (R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12),
  ] =

    ops =>
      new TupleBatch(
        (
          s1.sequence(ops._1),
          s2.sequence(ops._2),
          s3.sequence(ops._3),
          s4.sequence(ops._4),
          s5.sequence(ops._5),
          s6.sequence(ops._6),
          s7.sequence(ops._7),
          s8.sequence(ops._8),
          s9.sequence(ops._9),
          s10.sequence(ops._10),
          s11.sequence(ops._11),
          s12.sequence(ops._12),
        ),
        arr =>
          (
            arr(0).asInstanceOf[R1],
            arr(1).asInstanceOf[R2],
            arr(2).asInstanceOf[R3],
            arr(3).asInstanceOf[R4],
            arr(4).asInstanceOf[R5],
            arr(5).asInstanceOf[R6],
            arr(6).asInstanceOf[R7],
            arr(7).asInstanceOf[R8],
            arr(8).asInstanceOf[R9],
            arr(9).asInstanceOf[R10],
            arr(10).asInstanceOf[R11],
            arr(11).asInstanceOf[R12],
          ),
      )

  implicit def tuple13Sequencer[
    O1,
    O2,
    O3,
    O4,
    O5,
    O6,
    O7,
    O8,
    O9,
    O10,
    O11,
    O12,
    O13,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
  ](implicit
    s1: Sequencer[O1, R1],
    s2: Sequencer[O2, R2],
    s3: Sequencer[O3, R3],
    s4: Sequencer[O4, R4],
    s5: Sequencer[O5, R5],
    s6: Sequencer[O6, R6],
    s7: Sequencer[O7, R7],
    s8: Sequencer[O8, R8],
    s9: Sequencer[O9, R9],
    s10: Sequencer[O10, R10],
    s11: Sequencer[O11, R11],
    s12: Sequencer[O12, R12],
    s13: Sequencer[O13, R13],
  ): Sequencer[
    (O1, O2, O3, O4, O5, O6, O7, O8, O9, O10, O11, O12, O13),
    (R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13),
  ] =

    ops =>
      new TupleBatch(
        (
          s1.sequence(ops._1),
          s2.sequence(ops._2),
          s3.sequence(ops._3),
          s4.sequence(ops._4),
          s5.sequence(ops._5),
          s6.sequence(ops._6),
          s7.sequence(ops._7),
          s8.sequence(ops._8),
          s9.sequence(ops._9),
          s10.sequence(ops._10),
          s11.sequence(ops._11),
          s12.sequence(ops._12),
          s13.sequence(ops._13),
        ),
        arr =>
          (
            arr(0).asInstanceOf[R1],
            arr(1).asInstanceOf[R2],
            arr(2).asInstanceOf[R3],
            arr(3).asInstanceOf[R4],
            arr(4).asInstanceOf[R5],
            arr(5).asInstanceOf[R6],
            arr(6).asInstanceOf[R7],
            arr(7).asInstanceOf[R8],
            arr(8).asInstanceOf[R9],
            arr(9).asInstanceOf[R10],
            arr(10).asInstanceOf[R11],
            arr(11).asInstanceOf[R12],
            arr(12).asInstanceOf[R13],
          ),
      )

  implicit def tuple14Sequencer[
    O1,
    O2,
    O3,
    O4,
    O5,
    O6,
    O7,
    O8,
    O9,
    O10,
    O11,
    O12,
    O13,
    O14,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
  ](implicit
    s1: Sequencer[O1, R1],
    s2: Sequencer[O2, R2],
    s3: Sequencer[O3, R3],
    s4: Sequencer[O4, R4],
    s5: Sequencer[O5, R5],
    s6: Sequencer[O6, R6],
    s7: Sequencer[O7, R7],
    s8: Sequencer[O8, R8],
    s9: Sequencer[O9, R9],
    s10: Sequencer[O10, R10],
    s11: Sequencer[O11, R11],
    s12: Sequencer[O12, R12],
    s13: Sequencer[O13, R13],
    s14: Sequencer[O14, R14],
  ): Sequencer[
    (O1, O2, O3, O4, O5, O6, O7, O8, O9, O10, O11, O12, O13, O14),
    (R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14),
  ] =

    ops =>
      new TupleBatch(
        (
          s1.sequence(ops._1),
          s2.sequence(ops._2),
          s3.sequence(ops._3),
          s4.sequence(ops._4),
          s5.sequence(ops._5),
          s6.sequence(ops._6),
          s7.sequence(ops._7),
          s8.sequence(ops._8),
          s9.sequence(ops._9),
          s10.sequence(ops._10),
          s11.sequence(ops._11),
          s12.sequence(ops._12),
          s13.sequence(ops._13),
          s14.sequence(ops._14),
        ),
        arr =>
          (
            arr(0).asInstanceOf[R1],
            arr(1).asInstanceOf[R2],
            arr(2).asInstanceOf[R3],
            arr(3).asInstanceOf[R4],
            arr(4).asInstanceOf[R5],
            arr(5).asInstanceOf[R6],
            arr(6).asInstanceOf[R7],
            arr(7).asInstanceOf[R8],
            arr(8).asInstanceOf[R9],
            arr(9).asInstanceOf[R10],
            arr(10).asInstanceOf[R11],
            arr(11).asInstanceOf[R12],
            arr(12).asInstanceOf[R13],
            arr(13).asInstanceOf[R14],
          ),
      )

  implicit def tuple15Sequencer[
    O1,
    O2,
    O3,
    O4,
    O5,
    O6,
    O7,
    O8,
    O9,
    O10,
    O11,
    O12,
    O13,
    O14,
    O15,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
  ](implicit
    s1: Sequencer[O1, R1],
    s2: Sequencer[O2, R2],
    s3: Sequencer[O3, R3],
    s4: Sequencer[O4, R4],
    s5: Sequencer[O5, R5],
    s6: Sequencer[O6, R6],
    s7: Sequencer[O7, R7],
    s8: Sequencer[O8, R8],
    s9: Sequencer[O9, R9],
    s10: Sequencer[O10, R10],
    s11: Sequencer[O11, R11],
    s12: Sequencer[O12, R12],
    s13: Sequencer[O13, R13],
    s14: Sequencer[O14, R14],
    s15: Sequencer[O15, R15],
  ): Sequencer[
    (O1, O2, O3, O4, O5, O6, O7, O8, O9, O10, O11, O12, O13, O14, O15),
    (R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15),
  ] =

    ops =>
      new TupleBatch(
        (
          s1.sequence(ops._1),
          s2.sequence(ops._2),
          s3.sequence(ops._3),
          s4.sequence(ops._4),
          s5.sequence(ops._5),
          s6.sequence(ops._6),
          s7.sequence(ops._7),
          s8.sequence(ops._8),
          s9.sequence(ops._9),
          s10.sequence(ops._10),
          s11.sequence(ops._11),
          s12.sequence(ops._12),
          s13.sequence(ops._13),
          s14.sequence(ops._14),
          s15.sequence(ops._15),
        ),
        arr =>
          (
            arr(0).asInstanceOf[R1],
            arr(1).asInstanceOf[R2],
            arr(2).asInstanceOf[R3],
            arr(3).asInstanceOf[R4],
            arr(4).asInstanceOf[R5],
            arr(5).asInstanceOf[R6],
            arr(6).asInstanceOf[R7],
            arr(7).asInstanceOf[R8],
            arr(8).asInstanceOf[R9],
            arr(9).asInstanceOf[R10],
            arr(10).asInstanceOf[R11],
            arr(11).asInstanceOf[R12],
            arr(12).asInstanceOf[R13],
            arr(13).asInstanceOf[R14],
            arr(14).asInstanceOf[R15],
          ),
      )

  implicit def tuple16Sequencer[
    O1,
    O2,
    O3,
    O4,
    O5,
    O6,
    O7,
    O8,
    O9,
    O10,
    O11,
    O12,
    O13,
    O14,
    O15,
    O16,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    R16,
  ](implicit
    s1: Sequencer[O1, R1],
    s2: Sequencer[O2, R2],
    s3: Sequencer[O3, R3],
    s4: Sequencer[O4, R4],
    s5: Sequencer[O5, R5],
    s6: Sequencer[O6, R6],
    s7: Sequencer[O7, R7],
    s8: Sequencer[O8, R8],
    s9: Sequencer[O9, R9],
    s10: Sequencer[O10, R10],
    s11: Sequencer[O11, R11],
    s12: Sequencer[O12, R12],
    s13: Sequencer[O13, R13],
    s14: Sequencer[O14, R14],
    s15: Sequencer[O15, R15],
    s16: Sequencer[O16, R16],
  ): Sequencer[
    (O1, O2, O3, O4, O5, O6, O7, O8, O9, O10, O11, O12, O13, O14, O15, O16),
    (R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15, R16),
  ] =

    ops =>
      new TupleBatch(
        (
          s1.sequence(ops._1),
          s2.sequence(ops._2),
          s3.sequence(ops._3),
          s4.sequence(ops._4),
          s5.sequence(ops._5),
          s6.sequence(ops._6),
          s7.sequence(ops._7),
          s8.sequence(ops._8),
          s9.sequence(ops._9),
          s10.sequence(ops._10),
          s11.sequence(ops._11),
          s12.sequence(ops._12),
          s13.sequence(ops._13),
          s14.sequence(ops._14),
          s15.sequence(ops._15),
          s16.sequence(ops._16),
        ),
        arr =>
          (
            arr(0).asInstanceOf[R1],
            arr(1).asInstanceOf[R2],
            arr(2).asInstanceOf[R3],
            arr(3).asInstanceOf[R4],
            arr(4).asInstanceOf[R5],
            arr(5).asInstanceOf[R6],
            arr(6).asInstanceOf[R7],
            arr(7).asInstanceOf[R8],
            arr(8).asInstanceOf[R9],
            arr(9).asInstanceOf[R10],
            arr(10).asInstanceOf[R11],
            arr(11).asInstanceOf[R12],
            arr(12).asInstanceOf[R13],
            arr(13).asInstanceOf[R14],
            arr(14).asInstanceOf[R15],
            arr(15).asInstanceOf[R16],
          ),
      )

  implicit def tuple17Sequencer[
    O1,
    O2,
    O3,
    O4,
    O5,
    O6,
    O7,
    O8,
    O9,
    O10,
    O11,
    O12,
    O13,
    O14,
    O15,
    O16,
    O17,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    R16,
    R17,
  ](implicit
    s1: Sequencer[O1, R1],
    s2: Sequencer[O2, R2],
    s3: Sequencer[O3, R3],
    s4: Sequencer[O4, R4],
    s5: Sequencer[O5, R5],
    s6: Sequencer[O6, R6],
    s7: Sequencer[O7, R7],
    s8: Sequencer[O8, R8],
    s9: Sequencer[O9, R9],
    s10: Sequencer[O10, R10],
    s11: Sequencer[O11, R11],
    s12: Sequencer[O12, R12],
    s13: Sequencer[O13, R13],
    s14: Sequencer[O14, R14],
    s15: Sequencer[O15, R15],
    s16: Sequencer[O16, R16],
    s17: Sequencer[O17, R17],
  ): Sequencer[
    (O1, O2, O3, O4, O5, O6, O7, O8, O9, O10, O11, O12, O13, O14, O15, O16, O17),
    (R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15, R16, R17),
  ] =

    ops =>
      new TupleBatch(
        (
          s1.sequence(ops._1),
          s2.sequence(ops._2),
          s3.sequence(ops._3),
          s4.sequence(ops._4),
          s5.sequence(ops._5),
          s6.sequence(ops._6),
          s7.sequence(ops._7),
          s8.sequence(ops._8),
          s9.sequence(ops._9),
          s10.sequence(ops._10),
          s11.sequence(ops._11),
          s12.sequence(ops._12),
          s13.sequence(ops._13),
          s14.sequence(ops._14),
          s15.sequence(ops._15),
          s16.sequence(ops._16),
          s17.sequence(ops._17),
        ),
        arr =>
          (
            arr(0).asInstanceOf[R1],
            arr(1).asInstanceOf[R2],
            arr(2).asInstanceOf[R3],
            arr(3).asInstanceOf[R4],
            arr(4).asInstanceOf[R5],
            arr(5).asInstanceOf[R6],
            arr(6).asInstanceOf[R7],
            arr(7).asInstanceOf[R8],
            arr(8).asInstanceOf[R9],
            arr(9).asInstanceOf[R10],
            arr(10).asInstanceOf[R11],
            arr(11).asInstanceOf[R12],
            arr(12).asInstanceOf[R13],
            arr(13).asInstanceOf[R14],
            arr(14).asInstanceOf[R15],
            arr(15).asInstanceOf[R16],
            arr(16).asInstanceOf[R17],
          ),
      )

  implicit def tuple18Sequencer[
    O1,
    O2,
    O3,
    O4,
    O5,
    O6,
    O7,
    O8,
    O9,
    O10,
    O11,
    O12,
    O13,
    O14,
    O15,
    O16,
    O17,
    O18,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    R16,
    R17,
    R18,
  ](implicit
    s1: Sequencer[O1, R1],
    s2: Sequencer[O2, R2],
    s3: Sequencer[O3, R3],
    s4: Sequencer[O4, R4],
    s5: Sequencer[O5, R5],
    s6: Sequencer[O6, R6],
    s7: Sequencer[O7, R7],
    s8: Sequencer[O8, R8],
    s9: Sequencer[O9, R9],
    s10: Sequencer[O10, R10],
    s11: Sequencer[O11, R11],
    s12: Sequencer[O12, R12],
    s13: Sequencer[O13, R13],
    s14: Sequencer[O14, R14],
    s15: Sequencer[O15, R15],
    s16: Sequencer[O16, R16],
    s17: Sequencer[O17, R17],
    s18: Sequencer[O18, R18],
  ): Sequencer[
    (O1, O2, O3, O4, O5, O6, O7, O8, O9, O10, O11, O12, O13, O14, O15, O16, O17, O18),
    (R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15, R16, R17, R18),
  ] =

    ops =>
      new TupleBatch(
        (
          s1.sequence(ops._1),
          s2.sequence(ops._2),
          s3.sequence(ops._3),
          s4.sequence(ops._4),
          s5.sequence(ops._5),
          s6.sequence(ops._6),
          s7.sequence(ops._7),
          s8.sequence(ops._8),
          s9.sequence(ops._9),
          s10.sequence(ops._10),
          s11.sequence(ops._11),
          s12.sequence(ops._12),
          s13.sequence(ops._13),
          s14.sequence(ops._14),
          s15.sequence(ops._15),
          s16.sequence(ops._16),
          s17.sequence(ops._17),
          s18.sequence(ops._18),
        ),
        arr =>
          (
            arr(0).asInstanceOf[R1],
            arr(1).asInstanceOf[R2],
            arr(2).asInstanceOf[R3],
            arr(3).asInstanceOf[R4],
            arr(4).asInstanceOf[R5],
            arr(5).asInstanceOf[R6],
            arr(6).asInstanceOf[R7],
            arr(7).asInstanceOf[R8],
            arr(8).asInstanceOf[R9],
            arr(9).asInstanceOf[R10],
            arr(10).asInstanceOf[R11],
            arr(11).asInstanceOf[R12],
            arr(12).asInstanceOf[R13],
            arr(13).asInstanceOf[R14],
            arr(14).asInstanceOf[R15],
            arr(15).asInstanceOf[R16],
            arr(16).asInstanceOf[R17],
            arr(17).asInstanceOf[R18],
          ),
      )

  implicit def tuple19Sequencer[
    O1,
    O2,
    O3,
    O4,
    O5,
    O6,
    O7,
    O8,
    O9,
    O10,
    O11,
    O12,
    O13,
    O14,
    O15,
    O16,
    O17,
    O18,
    O19,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    R16,
    R17,
    R18,
    R19,
  ](implicit
    s1: Sequencer[O1, R1],
    s2: Sequencer[O2, R2],
    s3: Sequencer[O3, R3],
    s4: Sequencer[O4, R4],
    s5: Sequencer[O5, R5],
    s6: Sequencer[O6, R6],
    s7: Sequencer[O7, R7],
    s8: Sequencer[O8, R8],
    s9: Sequencer[O9, R9],
    s10: Sequencer[O10, R10],
    s11: Sequencer[O11, R11],
    s12: Sequencer[O12, R12],
    s13: Sequencer[O13, R13],
    s14: Sequencer[O14, R14],
    s15: Sequencer[O15, R15],
    s16: Sequencer[O16, R16],
    s17: Sequencer[O17, R17],
    s18: Sequencer[O18, R18],
    s19: Sequencer[O19, R19],
  ): Sequencer[
    (O1, O2, O3, O4, O5, O6, O7, O8, O9, O10, O11, O12, O13, O14, O15, O16, O17, O18, O19),
    (R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15, R16, R17, R18, R19),
  ] =

    ops =>
      new TupleBatch(
        (
          s1.sequence(ops._1),
          s2.sequence(ops._2),
          s3.sequence(ops._3),
          s4.sequence(ops._4),
          s5.sequence(ops._5),
          s6.sequence(ops._6),
          s7.sequence(ops._7),
          s8.sequence(ops._8),
          s9.sequence(ops._9),
          s10.sequence(ops._10),
          s11.sequence(ops._11),
          s12.sequence(ops._12),
          s13.sequence(ops._13),
          s14.sequence(ops._14),
          s15.sequence(ops._15),
          s16.sequence(ops._16),
          s17.sequence(ops._17),
          s18.sequence(ops._18),
          s19.sequence(ops._19),
        ),
        arr =>
          (
            arr(0).asInstanceOf[R1],
            arr(1).asInstanceOf[R2],
            arr(2).asInstanceOf[R3],
            arr(3).asInstanceOf[R4],
            arr(4).asInstanceOf[R5],
            arr(5).asInstanceOf[R6],
            arr(6).asInstanceOf[R7],
            arr(7).asInstanceOf[R8],
            arr(8).asInstanceOf[R9],
            arr(9).asInstanceOf[R10],
            arr(10).asInstanceOf[R11],
            arr(11).asInstanceOf[R12],
            arr(12).asInstanceOf[R13],
            arr(13).asInstanceOf[R14],
            arr(14).asInstanceOf[R15],
            arr(15).asInstanceOf[R16],
            arr(16).asInstanceOf[R17],
            arr(17).asInstanceOf[R18],
            arr(18).asInstanceOf[R19],
          ),
      )

  implicit def tuple20Sequencer[
    O1,
    O2,
    O3,
    O4,
    O5,
    O6,
    O7,
    O8,
    O9,
    O10,
    O11,
    O12,
    O13,
    O14,
    O15,
    O16,
    O17,
    O18,
    O19,
    O20,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    R16,
    R17,
    R18,
    R19,
    R20,
  ](implicit
    s1: Sequencer[O1, R1],
    s2: Sequencer[O2, R2],
    s3: Sequencer[O3, R3],
    s4: Sequencer[O4, R4],
    s5: Sequencer[O5, R5],
    s6: Sequencer[O6, R6],
    s7: Sequencer[O7, R7],
    s8: Sequencer[O8, R8],
    s9: Sequencer[O9, R9],
    s10: Sequencer[O10, R10],
    s11: Sequencer[O11, R11],
    s12: Sequencer[O12, R12],
    s13: Sequencer[O13, R13],
    s14: Sequencer[O14, R14],
    s15: Sequencer[O15, R15],
    s16: Sequencer[O16, R16],
    s17: Sequencer[O17, R17],
    s18: Sequencer[O18, R18],
    s19: Sequencer[O19, R19],
    s20: Sequencer[O20, R20],
  ): Sequencer[
    (O1, O2, O3, O4, O5, O6, O7, O8, O9, O10, O11, O12, O13, O14, O15, O16, O17, O18, O19, O20),
    (R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15, R16, R17, R18, R19, R20),
  ] =

    ops =>
      new TupleBatch(
        (
          s1.sequence(ops._1),
          s2.sequence(ops._2),
          s3.sequence(ops._3),
          s4.sequence(ops._4),
          s5.sequence(ops._5),
          s6.sequence(ops._6),
          s7.sequence(ops._7),
          s8.sequence(ops._8),
          s9.sequence(ops._9),
          s10.sequence(ops._10),
          s11.sequence(ops._11),
          s12.sequence(ops._12),
          s13.sequence(ops._13),
          s14.sequence(ops._14),
          s15.sequence(ops._15),
          s16.sequence(ops._16),
          s17.sequence(ops._17),
          s18.sequence(ops._18),
          s19.sequence(ops._19),
          s20.sequence(ops._20),
        ),
        arr =>
          (
            arr(0).asInstanceOf[R1],
            arr(1).asInstanceOf[R2],
            arr(2).asInstanceOf[R3],
            arr(3).asInstanceOf[R4],
            arr(4).asInstanceOf[R5],
            arr(5).asInstanceOf[R6],
            arr(6).asInstanceOf[R7],
            arr(7).asInstanceOf[R8],
            arr(8).asInstanceOf[R9],
            arr(9).asInstanceOf[R10],
            arr(10).asInstanceOf[R11],
            arr(11).asInstanceOf[R12],
            arr(12).asInstanceOf[R13],
            arr(13).asInstanceOf[R14],
            arr(14).asInstanceOf[R15],
            arr(15).asInstanceOf[R16],
            arr(16).asInstanceOf[R17],
            arr(17).asInstanceOf[R18],
            arr(18).asInstanceOf[R19],
            arr(19).asInstanceOf[R20],
          ),
      )

  implicit def tuple21Sequencer[
    O1,
    O2,
    O3,
    O4,
    O5,
    O6,
    O7,
    O8,
    O9,
    O10,
    O11,
    O12,
    O13,
    O14,
    O15,
    O16,
    O17,
    O18,
    O19,
    O20,
    O21,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    R16,
    R17,
    R18,
    R19,
    R20,
    R21,
  ](implicit
    s1: Sequencer[O1, R1],
    s2: Sequencer[O2, R2],
    s3: Sequencer[O3, R3],
    s4: Sequencer[O4, R4],
    s5: Sequencer[O5, R5],
    s6: Sequencer[O6, R6],
    s7: Sequencer[O7, R7],
    s8: Sequencer[O8, R8],
    s9: Sequencer[O9, R9],
    s10: Sequencer[O10, R10],
    s11: Sequencer[O11, R11],
    s12: Sequencer[O12, R12],
    s13: Sequencer[O13, R13],
    s14: Sequencer[O14, R14],
    s15: Sequencer[O15, R15],
    s16: Sequencer[O16, R16],
    s17: Sequencer[O17, R17],
    s18: Sequencer[O18, R18],
    s19: Sequencer[O19, R19],
    s20: Sequencer[O20, R20],
    s21: Sequencer[O21, R21],
  ): Sequencer[
    (O1, O2, O3, O4, O5, O6, O7, O8, O9, O10, O11, O12, O13, O14, O15, O16, O17, O18, O19, O20, O21),
    (R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15, R16, R17, R18, R19, R20, R21),
  ] =

    ops =>
      new TupleBatch(
        (
          s1.sequence(ops._1),
          s2.sequence(ops._2),
          s3.sequence(ops._3),
          s4.sequence(ops._4),
          s5.sequence(ops._5),
          s6.sequence(ops._6),
          s7.sequence(ops._7),
          s8.sequence(ops._8),
          s9.sequence(ops._9),
          s10.sequence(ops._10),
          s11.sequence(ops._11),
          s12.sequence(ops._12),
          s13.sequence(ops._13),
          s14.sequence(ops._14),
          s15.sequence(ops._15),
          s16.sequence(ops._16),
          s17.sequence(ops._17),
          s18.sequence(ops._18),
          s19.sequence(ops._19),
          s20.sequence(ops._20),
          s21.sequence(ops._21),
        ),
        arr =>
          (
            arr(0).asInstanceOf[R1],
            arr(1).asInstanceOf[R2],
            arr(2).asInstanceOf[R3],
            arr(3).asInstanceOf[R4],
            arr(4).asInstanceOf[R5],
            arr(5).asInstanceOf[R6],
            arr(6).asInstanceOf[R7],
            arr(7).asInstanceOf[R8],
            arr(8).asInstanceOf[R9],
            arr(9).asInstanceOf[R10],
            arr(10).asInstanceOf[R11],
            arr(11).asInstanceOf[R12],
            arr(12).asInstanceOf[R13],
            arr(13).asInstanceOf[R14],
            arr(14).asInstanceOf[R15],
            arr(15).asInstanceOf[R16],
            arr(16).asInstanceOf[R17],
            arr(17).asInstanceOf[R18],
            arr(18).asInstanceOf[R19],
            arr(19).asInstanceOf[R20],
            arr(20).asInstanceOf[R21],
          ),
      )

  implicit def tuple22Sequencer[
    O1,
    O2,
    O3,
    O4,
    O5,
    O6,
    O7,
    O8,
    O9,
    O10,
    O11,
    O12,
    O13,
    O14,
    O15,
    O16,
    O17,
    O18,
    O19,
    O20,
    O21,
    O22,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    R16,
    R17,
    R18,
    R19,
    R20,
    R21,
    R22,
  ](implicit
    s1: Sequencer[O1, R1],
    s2: Sequencer[O2, R2],
    s3: Sequencer[O3, R3],
    s4: Sequencer[O4, R4],
    s5: Sequencer[O5, R5],
    s6: Sequencer[O6, R6],
    s7: Sequencer[O7, R7],
    s8: Sequencer[O8, R8],
    s9: Sequencer[O9, R9],
    s10: Sequencer[O10, R10],
    s11: Sequencer[O11, R11],
    s12: Sequencer[O12, R12],
    s13: Sequencer[O13, R13],
    s14: Sequencer[O14, R14],
    s15: Sequencer[O15, R15],
    s16: Sequencer[O16, R16],
    s17: Sequencer[O17, R17],
    s18: Sequencer[O18, R18],
    s19: Sequencer[O19, R19],
    s20: Sequencer[O20, R20],
    s21: Sequencer[O21, R21],
    s22: Sequencer[O22, R22],
  ): Sequencer[
    (O1, O2, O3, O4, O5, O6, O7, O8, O9, O10, O11, O12, O13, O14, O15, O16, O17, O18, O19, O20, O21, O22),
    (R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15, R16, R17, R18, R19, R20, R21, R22),
  ] =

    ops =>
      new TupleBatch(
        (
          s1.sequence(ops._1),
          s2.sequence(ops._2),
          s3.sequence(ops._3),
          s4.sequence(ops._4),
          s5.sequence(ops._5),
          s6.sequence(ops._6),
          s7.sequence(ops._7),
          s8.sequence(ops._8),
          s9.sequence(ops._9),
          s10.sequence(ops._10),
          s11.sequence(ops._11),
          s12.sequence(ops._12),
          s13.sequence(ops._13),
          s14.sequence(ops._14),
          s15.sequence(ops._15),
          s16.sequence(ops._16),
          s17.sequence(ops._17),
          s18.sequence(ops._18),
          s19.sequence(ops._19),
          s20.sequence(ops._20),
          s21.sequence(ops._21),
          s22.sequence(ops._22),
        ),
        arr =>
          (
            arr(0).asInstanceOf[R1],
            arr(1).asInstanceOf[R2],
            arr(2).asInstanceOf[R3],
            arr(3).asInstanceOf[R4],
            arr(4).asInstanceOf[R5],
            arr(5).asInstanceOf[R6],
            arr(6).asInstanceOf[R7],
            arr(7).asInstanceOf[R8],
            arr(8).asInstanceOf[R9],
            arr(9).asInstanceOf[R10],
            arr(10).asInstanceOf[R11],
            arr(11).asInstanceOf[R12],
            arr(12).asInstanceOf[R13],
            arr(13).asInstanceOf[R14],
            arr(14).asInstanceOf[R15],
            arr(15).asInstanceOf[R16],
            arr(16).asInstanceOf[R17],
            arr(17).asInstanceOf[R18],
            arr(18).asInstanceOf[R19],
            arr(19).asInstanceOf[R20],
            arr(20).asInstanceOf[R21],
            arr(21).asInstanceOf[R22],
          ),
      )
}

object TupleSequencers {
  private class TupleBatch[T](batches: Product, fun: Array[Any] => T) extends RedisBatch[T] with RawCommandPacks {
    def rawCommandPacks: TupleBatch[T] = this
    def emitCommandPacks(consumer: RawCommandPack => Unit): Unit = {
      var i = 0
      while (i < batches.productArity) {
        batches.productElement(i).asInstanceOf[RedisBatch[Any]].rawCommandPacks.emitCommandPacks(consumer)
        i += 1
      }
    }
    def computeSize(limit: Int): Int = {
      var res = 0
      var i = 0
      while (i < batches.productArity && res < limit) {
        res += batches.productElement(i).asInstanceOf[RedisBatch[Any]].rawCommandPacks.computeSize(limit - res)
        i += 1
      }
      res
    }
    def decodeReplies(replies: Int => RedisReply, index: Index, inTransaction: Boolean): T = {
      val result = new Array[Any](batches.productArity)
      var failure: Opt[Throwable] = Opt.Empty
      var i = 0
      while (i < batches.productArity) {
        try {
          result(i) =
            batches.productElement(i).asInstanceOf[RedisBatch[Any]].decodeReplies(replies, index, inTransaction)
        } catch {
          case NonFatal(cause) =>
            failure = failure orElse cause.opt
        }
        i += 1
      }
      failure.fold(fun(result))(throw _)
    }
  }
}

object Gen {
  def main(args: Array[String]): Unit = {
    for (n <- 2 to 22) {
      val is = 1 to n
      val os = is.map(i => s"O$i").mkString(", ")
      val rs = is.map(i => s"R$i").mkString(", ")
      val sequencers = is.map(i => s"s$i: Sequencer[O$i, R$i]").mkString(",\n")
      val batches = is.map(i => s"s$i.sequence(ops._$i)").mkString(",\n")
      val result = is.map(i => s"arr(${i - 1}).asInstanceOf[R$i]").mkString(",\n")

      val code =
        s"""
           |implicit def tuple${n}Sequencer[$os, $rs](implicit
           |  $sequencers): Sequencer[($os), ($rs)] =
           |
           |  new Sequencer[($os), ($rs)] {
           |    def sequence(ops: ($os)) = new TupleBatch((
           |      $batches),
           |      arr => (
           |        $result)
           |    )
           |  }
        """.stripMargin

      println(code)
    }
  }
}
