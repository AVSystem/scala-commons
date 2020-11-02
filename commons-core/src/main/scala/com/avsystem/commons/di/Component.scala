package com.avsystem.commons
package di

import java.util.concurrent.atomic.AtomicReference

import com.avsystem.commons.macros.di.ComponentMacros

import scala.annotation.compileTimeOnly

case class ComponentName(name: String) extends AnyVal {
  override def toString: String = name
}
object ComponentName {
  @compileTimeOnly("implicit ComponentName is only available inside argument of Component(...) macro")
  implicit def componentName: ComponentName = sys.error("stub")
}

abstract class Component[+T] {
  def name: String

  private[this] val savedFuture = new AtomicReference[Future[T]]

  @compileTimeOnly(".ref can only be used inside argument to Component(...) macro")
  def ref: T = sys.error("stub")

  def dependsOn(deps: Component[_]*): Component[T] =
    new Component.WithAdditionalDeps(this, deps)

  def init()(implicit ec: ExecutionContext): Future[T] =
    init(ComponentInitContext(ec, Nil))

  final def init(ctx: ComponentInitContext): Future[T] = try {
    implicit val ec: ExecutionContext = ctx.executor
    val newCtx = ctx.push(this)
    val promise = Promise[T]()
    if (savedFuture.compareAndSet(null, promise.future)) {
      val resultFuture = Future.traverse(dependencies)(_.init(newCtx)).map(deps => create(deps.toIndexedSeq))
      promise.completeWith(resultFuture)
    }
    savedFuture.get()
  } catch {
    case NonFatal(cause) => Future.failed(cause)
  }

  def dependencies: IndexedSeq[Component[_]]

  protected def create(resolvedDeps: IndexedSeq[Any]): T
}
object Component {
  def apply[T](definition: => T): Component[T] = macro ComponentMacros.componentApply[T]

  private class WithAdditionalDeps[T](wrapped: Component[T], deps: Seq[Component[_]]) extends Component[T] {
    def name: String = wrapped.name
    def dependencies: IndexedSeq[Component[_]] = wrapped.dependencies ++ deps
    protected def create(resolvedDeps: IndexedSeq[Any]): T = wrapped.create(resolvedDeps)
  }
}

case class ComponentCycleException(cyclePath: List[Component[_]])
  extends Exception(s"component dependency cycle detected: ${cyclePath.map(_.name).mkString(" -> ")}")

case class ComponentInitContext(
  executor: ExecutionContext,
  stack: List[Component[_]]
) {
  def push(component: Component[_]): ComponentInitContext = {
    if (stack.contains(component)) {
      throw ComponentCycleException((component :: stack.takeWhile(_ != component)).reverse)
    }
    ComponentInitContext(executor, component :: stack)
  }
}

trait Injectable extends Any
object Injectable {
  @compileTimeOnly("implicit Component[T] => implicit T inference only works inside argument to Component(...) macro")
  implicit def inject[T <: Injectable](implicit component: Component[T]): T = sys.error("stub")
}
