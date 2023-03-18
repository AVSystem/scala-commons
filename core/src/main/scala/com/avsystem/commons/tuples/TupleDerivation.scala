package com.avsystem.commons
package tuples

/**
  * IntelliJ-friendly (no whitebox macros) boilerplate strapping layer for deriving tuple type classes (`C`)
  * based on instances of that typeclass for tuple element types.
  */
trait TupleDerivation[C[_]] {
  case class ElementInstances[T, I](instances: I)
  object ElementInstances {
    implicit def tuple1Instances[T1](implicit i1: C[T1]): ElementInstances[Tuple1[T1], Tuple1[C[T1]]] =
      ElementInstances(Tuple1(i1))
    implicit def tuple2Instances[T1, T2](implicit i1: C[T1], i2: C[T2]): ElementInstances[(T1, T2), (C[T1], C[T2])] =
      ElementInstances((i1, i2))
    implicit def tuple3Instances[T1, T2, T3](implicit i1: C[T1], i2: C[T2], i3: C[T3]): ElementInstances[(T1, T2, T3), (C[T1], C[T2], C[T3])] =
      ElementInstances((i1, i2, i3))
    implicit def tuple4Instances[T1, T2, T3, T4](implicit i1: C[T1], i2: C[T2], i3: C[T3], i4: C[T4]): ElementInstances[(T1, T2, T3, T4), (C[T1], C[T2], C[T3], C[T4])] =
      ElementInstances((i1, i2, i3, i4))
    implicit def tuple5Instances[T1, T2, T3, T4, T5](implicit i1: C[T1], i2: C[T2], i3: C[T3], i4: C[T4], i5: C[T5]): ElementInstances[(T1, T2, T3, T4, T5), (C[T1], C[T2], C[T3], C[T4], C[T5])] =
      ElementInstances((i1, i2, i3, i4, i5))
    implicit def tuple6Instances[T1, T2, T3, T4, T5, T6](implicit i1: C[T1], i2: C[T2], i3: C[T3], i4: C[T4], i5: C[T5], i6: C[T6]): ElementInstances[(T1, T2, T3, T4, T5, T6), (C[T1], C[T2], C[T3], C[T4], C[T5], C[T6])] =
      ElementInstances((i1, i2, i3, i4, i5, i6))
    implicit def tuple7Instances[T1, T2, T3, T4, T5, T6, T7](implicit i1: C[T1], i2: C[T2], i3: C[T3], i4: C[T4], i5: C[T5], i6: C[T6], i7: C[T7]): ElementInstances[(T1, T2, T3, T4, T5, T6, T7), (C[T1], C[T2], C[T3], C[T4], C[T5], C[T6], C[T7])] =
      ElementInstances((i1, i2, i3, i4, i5, i6, i7))
    implicit def tuple8Instances[T1, T2, T3, T4, T5, T6, T7, T8](implicit i1: C[T1], i2: C[T2], i3: C[T3], i4: C[T4], i5: C[T5], i6: C[T6], i7: C[T7], i8: C[T8]): ElementInstances[(T1, T2, T3, T4, T5, T6, T7, T8), (C[T1], C[T2], C[T3], C[T4], C[T5], C[T6], C[T7], C[T8])] =
      ElementInstances((i1, i2, i3, i4, i5, i6, i7, i8))
    implicit def tuple9Instances[T1, T2, T3, T4, T5, T6, T7, T8, T9](implicit i1: C[T1], i2: C[T2], i3: C[T3], i4: C[T4], i5: C[T5], i6: C[T6], i7: C[T7], i8: C[T8], i9: C[T9]): ElementInstances[(T1, T2, T3, T4, T5, T6, T7, T8, T9), (C[T1], C[T2], C[T3], C[T4], C[T5], C[T6], C[T7], C[T8], C[T9])] =
      ElementInstances((i1, i2, i3, i4, i5, i6, i7, i8, i9))
    implicit def tuple10Instances[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10](implicit i1: C[T1], i2: C[T2], i3: C[T3], i4: C[T4], i5: C[T5], i6: C[T6], i7: C[T7], i8: C[T8], i9: C[T9], i10: C[T10]): ElementInstances[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10), (C[T1], C[T2], C[T3], C[T4], C[T5], C[T6], C[T7], C[T8], C[T9], C[T10])] =
      ElementInstances((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10))
    implicit def tuple11Instances[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11](implicit i1: C[T1], i2: C[T2], i3: C[T3], i4: C[T4], i5: C[T5], i6: C[T6], i7: C[T7], i8: C[T8], i9: C[T9], i10: C[T10], i11: C[T11]): ElementInstances[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11), (C[T1], C[T2], C[T3], C[T4], C[T5], C[T6], C[T7], C[T8], C[T9], C[T10], C[T11])] =
      ElementInstances((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11))
    implicit def tuple12Instances[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12](implicit i1: C[T1], i2: C[T2], i3: C[T3], i4: C[T4], i5: C[T5], i6: C[T6], i7: C[T7], i8: C[T8], i9: C[T9], i10: C[T10], i11: C[T11], i12: C[T12]): ElementInstances[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12), (C[T1], C[T2], C[T3], C[T4], C[T5], C[T6], C[T7], C[T8], C[T9], C[T10], C[T11], C[T12])] =
      ElementInstances((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12))
    implicit def tuple13Instances[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13](implicit i1: C[T1], i2: C[T2], i3: C[T3], i4: C[T4], i5: C[T5], i6: C[T6], i7: C[T7], i8: C[T8], i9: C[T9], i10: C[T10], i11: C[T11], i12: C[T12], i13: C[T13]): ElementInstances[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13), (C[T1], C[T2], C[T3], C[T4], C[T5], C[T6], C[T7], C[T8], C[T9], C[T10], C[T11], C[T12], C[T13])] =
      ElementInstances((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13))
    implicit def tuple14Instances[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14](implicit i1: C[T1], i2: C[T2], i3: C[T3], i4: C[T4], i5: C[T5], i6: C[T6], i7: C[T7], i8: C[T8], i9: C[T9], i10: C[T10], i11: C[T11], i12: C[T12], i13: C[T13], i14: C[T14]): ElementInstances[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14), (C[T1], C[T2], C[T3], C[T4], C[T5], C[T6], C[T7], C[T8], C[T9], C[T10], C[T11], C[T12], C[T13], C[T14])] =
      ElementInstances((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14))
    implicit def tuple15Instances[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15](implicit i1: C[T1], i2: C[T2], i3: C[T3], i4: C[T4], i5: C[T5], i6: C[T6], i7: C[T7], i8: C[T8], i9: C[T9], i10: C[T10], i11: C[T11], i12: C[T12], i13: C[T13], i14: C[T14], i15: C[T15]): ElementInstances[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15), (C[T1], C[T2], C[T3], C[T4], C[T5], C[T6], C[T7], C[T8], C[T9], C[T10], C[T11], C[T12], C[T13], C[T14], C[T15])] =
      ElementInstances((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15))
    implicit def tuple16Instances[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16](implicit i1: C[T1], i2: C[T2], i3: C[T3], i4: C[T4], i5: C[T5], i6: C[T6], i7: C[T7], i8: C[T8], i9: C[T9], i10: C[T10], i11: C[T11], i12: C[T12], i13: C[T13], i14: C[T14], i15: C[T15], i16: C[T16]): ElementInstances[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16), (C[T1], C[T2], C[T3], C[T4], C[T5], C[T6], C[T7], C[T8], C[T9], C[T10], C[T11], C[T12], C[T13], C[T14], C[T15], C[T16])] =
      ElementInstances((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16))
    implicit def tuple17Instances[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17](implicit i1: C[T1], i2: C[T2], i3: C[T3], i4: C[T4], i5: C[T5], i6: C[T6], i7: C[T7], i8: C[T8], i9: C[T9], i10: C[T10], i11: C[T11], i12: C[T12], i13: C[T13], i14: C[T14], i15: C[T15], i16: C[T16], i17: C[T17]): ElementInstances[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17), (C[T1], C[T2], C[T3], C[T4], C[T5], C[T6], C[T7], C[T8], C[T9], C[T10], C[T11], C[T12], C[T13], C[T14], C[T15], C[T16], C[T17])] =
      ElementInstances((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17))
    implicit def tuple18Instances[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18](implicit i1: C[T1], i2: C[T2], i3: C[T3], i4: C[T4], i5: C[T5], i6: C[T6], i7: C[T7], i8: C[T8], i9: C[T9], i10: C[T10], i11: C[T11], i12: C[T12], i13: C[T13], i14: C[T14], i15: C[T15], i16: C[T16], i17: C[T17], i18: C[T18]): ElementInstances[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18), (C[T1], C[T2], C[T3], C[T4], C[T5], C[T6], C[T7], C[T8], C[T9], C[T10], C[T11], C[T12], C[T13], C[T14], C[T15], C[T16], C[T17], C[T18])] =
      ElementInstances((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18))
    implicit def tuple19Instances[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19](implicit i1: C[T1], i2: C[T2], i3: C[T3], i4: C[T4], i5: C[T5], i6: C[T6], i7: C[T7], i8: C[T8], i9: C[T9], i10: C[T10], i11: C[T11], i12: C[T12], i13: C[T13], i14: C[T14], i15: C[T15], i16: C[T16], i17: C[T17], i18: C[T18], i19: C[T19]): ElementInstances[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19), (C[T1], C[T2], C[T3], C[T4], C[T5], C[T6], C[T7], C[T8], C[T9], C[T10], C[T11], C[T12], C[T13], C[T14], C[T15], C[T16], C[T17], C[T18], C[T19])] =
      ElementInstances((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19))
    implicit def tuple20Instances[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20](implicit i1: C[T1], i2: C[T2], i3: C[T3], i4: C[T4], i5: C[T5], i6: C[T6], i7: C[T7], i8: C[T8], i9: C[T9], i10: C[T10], i11: C[T11], i12: C[T12], i13: C[T13], i14: C[T14], i15: C[T15], i16: C[T16], i17: C[T17], i18: C[T18], i19: C[T19], i20: C[T20]): ElementInstances[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20), (C[T1], C[T2], C[T3], C[T4], C[T5], C[T6], C[T7], C[T8], C[T9], C[T10], C[T11], C[T12], C[T13], C[T14], C[T15], C[T16], C[T17], C[T18], C[T19], C[T20])] =
      ElementInstances((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20))
    implicit def tuple21Instances[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21](implicit i1: C[T1], i2: C[T2], i3: C[T3], i4: C[T4], i5: C[T5], i6: C[T6], i7: C[T7], i8: C[T8], i9: C[T9], i10: C[T10], i11: C[T11], i12: C[T12], i13: C[T13], i14: C[T14], i15: C[T15], i16: C[T16], i17: C[T17], i18: C[T18], i19: C[T19], i20: C[T20], i21: C[T21]): ElementInstances[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21), (C[T1], C[T2], C[T3], C[T4], C[T5], C[T6], C[T7], C[T8], C[T9], C[T10], C[T11], C[T12], C[T13], C[T14], C[T15], C[T16], C[T17], C[T18], C[T19], C[T20], C[T21])] =
      ElementInstances((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21))
    implicit def tuple22Instances[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22](implicit i1: C[T1], i2: C[T2], i3: C[T3], i4: C[T4], i5: C[T5], i6: C[T6], i7: C[T7], i8: C[T8], i9: C[T9], i10: C[T10], i11: C[T11], i12: C[T12], i13: C[T13], i14: C[T14], i15: C[T15], i16: C[T16], i17: C[T17], i18: C[T18], i19: C[T19], i20: C[T20], i21: C[T21], i22: C[T22]): ElementInstances[(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22), (C[T1], C[T2], C[T3], C[T4], C[T5], C[T6], C[T7], C[T8], C[T9], C[T10], C[T11], C[T12], C[T13], C[T14], C[T15], C[T16], C[T17], C[T18], C[T19], C[T20], C[T21], C[T22])] =
      ElementInstances((i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19, i20, i21, i22))
  }
}
object GenTupleDerivation {
  def main(args: Array[String]): Unit = {
    for (i <- 1 to 22) {
      val js = 1 to i
      val tuple = js.map(j => s"T$j").mkString(",")
      val instParams = js.map(j => s"i$j: C[T$j]").mkString(",")
      val instTypes = js.map(j => s"C[T$j]").mkString(",")
      val instances = js.map(j => s"i$j").mkString(",")
      println(s"def tuple${i}Instances[$tuple](implicit $instParams): ElementInstances[($tuple),($instTypes)] =\nElementInstances(($instances))")
    }
  }
}
