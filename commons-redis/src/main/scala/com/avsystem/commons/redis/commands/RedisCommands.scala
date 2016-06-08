package com.avsystem.commons
package redis.commands

import akka.util.Timeout
import com.avsystem.commons.redis._

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

class RedisClusterCommands[+F[_]] private(protected val mapper: PolyFun[ClusterCommand, F]) extends RedisApi[F] {
  type CmdScope = Scope.Cluster
  type Self[G[_]] = RedisClusterCommands[G]
  protected def withMapper[G[_]](mapper: PolyFun[ClusterCommand, G]) = new RedisClusterCommands(mapper)
}
object RedisClusterCommands extends RedisClusterCommands[ClusterCommand](PolyFun.identity)

class RedisNodeCommands[+F[_]] private(protected val mapper: PolyFun[NodeCommand, F]) extends RedisNodeApi[F] {
  type CmdScope = Scope.Node
  type Self[G[_]] = RedisNodeCommands[G]
  protected def withMapper[G[_]](mapper: PolyFun[NodeCommand, G]) = new RedisNodeCommands(mapper)
}
object RedisNodeCommands extends RedisNodeCommands[NodeCommand](PolyFun.identity)

class RedisConnectionCommands[+F[_]] private(protected val mapper: PolyFun[ConnectionCommand, F]) extends RedisConnectionApi[F] {
  type CmdScope = Scope.Connection
  type Self[G[_]] = RedisConnectionCommands[G]
  protected def withMapper[G[_]](mapper: PolyFun[ConnectionCommand, G]) = new RedisConnectionCommands(mapper)
}
object RedisConnectionCommands extends RedisConnectionCommands[ConnectionCommand](PolyFun.identity)

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
  type CmdScope
  type Command[+A] = RedisCommand[A, CmdScope]
  type Self[G[_]] <: ApiSubset[G]

  protected def mapper: PolyFun[Command, F]
  protected def withMapper[G[_]](mapper: PolyFun[Command, G]): Self[G]

  def transform[G[_]](pf: PolyFun[F, G]): Self[G] =
    withMapper(mapper andThen pf)
}

object ApiSubset {
  implicit class BatchApiSubset[S, C <: ApiSubset[NodeCommand]](val cs: C) extends AnyVal {
    def operations: cs.Self[NodeOp] = cs.transform[NodeOp](new Mappers.BatchToOp[Scope.Node])
  }

  implicit class OperationsApiSubset[C <: ApiSubset[NodeOp]](val cs: C) extends AnyVal {
    def executedWith(client: RedisNodeClient)(implicit timeout: Timeout): cs.Self[Future] =
      cs.transform(new Mappers.ExecuteWith(client))
  }
}

trait ClusterApiSubset[+F[_]] extends ApiSubset[F] {
  type CmdScope >: Scope.Cluster
}

trait NodeApiSubset[+F[_]] extends ClusterApiSubset[F] {
  type CmdScope >: Scope.Node
}

trait ConnectionApiSubset[+F[_]] extends NodeApiSubset[F] {
  type CmdScope >: Scope.Connection
}

object Mappers {
  final class BatchToOp[S] extends PolyFun[Scoped[S]#Batch, Scoped[S]#Op] {
    def apply[A](f: Scoped[S]#Batch[A]) = f.operation
  }
  final class ExecuteWith(client: RedisNodeClient)(implicit timeout: Timeout) extends PolyFun[Scoped[Scope.Node]#Op, Future] {
    def apply[T](op: Scoped[Scope.Node]#Op[T]) = client.execute(op)
  }
}
