package com.avsystem.commons
package redis

trait TupleSequencers {this: Sequencer.type =>
  implicit def tuple2Sequencer[O1, O2, R1, R2, S](implicit
    s1: Sequencer[O1, R1, S],
    s2: Sequencer[O2, R2, S]): Sequencer[(O1, O2), (R1, R2), S] =

    new Sequencer[(O1, O2), (R1, R2), S] {
      def sequence(ops: (O1, O2)) =
        (s1.sequence(ops._1) map2 s2.sequence(ops._2)) ((r1, r2) => (r1, r2))
    }
}
