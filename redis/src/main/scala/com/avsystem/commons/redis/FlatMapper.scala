package com.avsystem.commons
package redis

import com.avsystem.commons.redis.HasFlatMap.FlatMapOps
import com.avsystem.commons.redis.RedisOp.{FlatMappedOp, LeafOp}

/** Typeclass that steers flat-mapping of [[RedisBatch]]es and [[RedisOp]]s with each other. It could be defined simpler
  * as:
  * {{{
  *   trait FlatMapper[-L[_],-R[_]] {
  *     def flatMap[A,B](left: L[A])(rightFun: A => R[B]): RedisOp[B]
  *   }
  * }}}
  * but unfortunately IntelliJ Scala plugin (3.0.7.31) does not understand that well (in `flatMap` callsites)
  */
trait FlatMapper[A, B, -L, -R] {
  def flatMap(left: L)(rightFun: A => R): RedisOp[B]
}

object FlatMapper {
  given[A, B] => FlatMapper[A, B, RedisOp[A], RedisOp[B]] =
    new FlatMapper[A, B, RedisOp[A], RedisOp[B]] {
      def flatMap(left: RedisOp[A])(rightFun: A => RedisOp[B]): RedisOp[B] = left match {
        case LeafOp(batch) => FlatMappedOp(batch, rightFun)
        case fmop: FlatMappedOp[pA, A] => FlatMappedOp(fmop.batch, (a: pA) => flatMap(fmop.fun(a))(rightFun))
      }
    }
  given[A, B] => FlatMapper[A, B, RedisOp[A], RedisBatch[B]] =
    new FlatMapper[A, B, RedisOp[A], RedisBatch[B]] {
      def flatMap(left: RedisOp[A])(rightFun: A => RedisBatch[B]) =
        OpOp.flatMap(left)(a => rightFun(a).operation)
    }
  given[A, B] => FlatMapper[A, B, RedisBatch[A], RedisOp[B]] =
    new FlatMapper[A, B, RedisBatch[A], RedisOp[B]] {
      def flatMap(left: RedisBatch[A])(rightFun: A => RedisOp[B]) =
        OpOp.flatMap(left.operation)(rightFun)
    }
  given[A, B] => FlatMapper[A, B, RedisBatch[A], RedisBatch[B]] =
    new FlatMapper[A, B, RedisBatch[A], RedisBatch[B]] {
      def flatMap(left: RedisBatch[A])(rightFun: A => RedisBatch[B]) =
        OpOp.flatMap(left.operation)(a => rightFun(a).operation)
    }
}

trait HasFlatMap[L[_]] {
  extension[L, A](left: L) {
    def flatMap[R, B](rightFun: A => R)(implicit fm: FlatMapper[A, B, L, R]): RedisOp[B] =
      fm.flatMap(left)(rightFun)
  }
}
