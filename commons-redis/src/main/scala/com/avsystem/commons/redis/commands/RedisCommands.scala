package com.avsystem.commons
package redis.commands

import akka.util.Timeout
import com.avsystem.commons.redis.{RedisCommand, RedisFlushable, RedisNodeClient, RedisOp}

import scala.concurrent.Future

/**
  * Author: ghik
  * Created: 08/04/16.
  */
trait RedisApi[+F[_]] extends KeysApi[F] with StringsApi[F] {
  type Self[G[_]] <: RedisApi[G]
}
trait RedisNodeApi[+F[_]] extends RedisApi[F] with NodeKeysApi[F] {
  type Self[G[_]] <: RedisNodeApi[G]
}
trait RedisConnectionApi[+F[_]] extends RedisNodeApi[F] {
  type Self[G[_]] <: RedisConnectionApi[G]
}

class RedisCommands[+F[_]] private(protected val mapper: PolyFun[RedisCommand, F]) extends RedisApi[F] {
  type Self[G[_]] = RedisCommands[G]
  protected type Initial[A] = RedisCommand[A]
  protected def withMapper[G[_]](mapper: PolyFun[RedisCommand, G]) = new RedisCommands(mapper)
}
object RedisCommands extends RedisCommands[RedisCommand](PolyFun.identity)

class RedisNodeCommands[+F[_]] private(protected val mapper: PolyFun[RedisCommand, F]) extends RedisNodeApi[F] {
  type Self[G[_]] = RedisNodeCommands[G]
  protected type Initial[A] = RedisCommand[A]
  protected def withMapper[G[_]](mapper: PolyFun[RedisCommand, G]) = new RedisNodeCommands(mapper)
}
object RedisNodeCommands extends RedisNodeCommands[RedisCommand](PolyFun.identity)

class RedisConnectionCommands[+F[_]] private(protected val mapper: PolyFun[RedisCommand, F]) extends RedisConnectionApi[F] {
  type Self[G[_]] = RedisConnectionCommands[G]
  protected type Initial[A] = RedisCommand[A]
  protected def withMapper[G[_]](mapper: PolyFun[RedisCommand, G]) = new RedisConnectionCommands(mapper)
}
object RedisConnectionCommands extends RedisConnectionCommands[RedisCommand](PolyFun.identity)

trait PolyFun[-F[_], +G[_]] {self =>
  def apply[A](fa: F[A]): G[A]

  def andThen[H[_]](f: PolyFun[G, H]): PolyFun[F, H] =
    new PolyFun[F, H] {
      def apply[A](fa: F[A]) = f(self(fa))
    }

  def compose[H[_]](f: PolyFun[H, F]): PolyFun[H, G] =
    f andThen self
}
object PolyFun {
  private val reusableIdentity = new PolyFun[Any, Any] {
    def apply[A](fa: Any) = fa
  }

  def identity[F[_]]: PolyFun[F, F] =
    reusableIdentity.asInstanceOf[PolyFun[F, F]]
}

trait ApiSubset[+F[_]] {
  type Self[G[_]] <: ApiSubset[G]

  protected type Initial[A] >: RedisCommand[A]

  protected def mapper: PolyFun[Initial, F]
  protected def withMapper[G[_]](mapper: PolyFun[Initial, G]): Self[G]

  def transform[G[_]](pf: PolyFun[F, G]): Self[G] =
    withMapper(mapper andThen pf)
}

object ApiSubset {
  implicit class FlushableApiSubset[C <: ApiSubset[RedisFlushable]](val cs: C) extends AnyVal {
    def operations: cs.Self[RedisOp] = cs.transform(Mappers.FlushableToOp)
  }

  implicit class OperationsApiSubset[C <: ApiSubset[RedisOp]](val cs: C) extends AnyVal {
    def executedWith(client: RedisNodeClient)(implicit timeout: Timeout): cs.Self[Future] =
      cs.transform(Mappers.ExecuteWith(client))
  }
}

object Mappers {
  object FlushableToOp extends PolyFun[RedisFlushable, RedisOp] {
    def apply[T](f: RedisFlushable[T]) = RedisOp.LeafOp(f)
  }
  case class ExecuteWith(client: RedisNodeClient)(implicit timeout: Timeout) extends PolyFun[RedisOp, Future] {
    def apply[T](op: RedisOp[T]) = client.execute(op)
  }
}
