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

case class DependencyCycleException(cyclePath: List[Component[_]])
  extends Exception(s"component dependency cycle detected: ${cyclePath.map(_.name).mkString("", " -> ", cyclePath.head.name)}")

abstract class Component[+T] {
  def name: String

  private[this] val savedFuture = new AtomicReference[Future[T]]

  @compileTimeOnly(".ref can only be used inside argument to Component(...) macro")
  def ref: T = sys.error("stub")

  def dependsOn(deps: Component[_]*): Component[T] =
    new Component.WithAdditionalDeps(this, deps)

  final def init()(implicit ec: ExecutionContext): Future[T] = {
    detectCycles(Nil, new MHashSet)
    doInit
  }

  private def detectCycles(stack: List[Component[_]], visited: MHashSet[Component[_]]): Unit =
    if (!visited.contains(this)) {
      if (!stack.contains(this)) {
        val newStack = this :: stack
        dependencies.foreach(_.detectCycles(newStack, visited))
        visited.add(this)
      } else {
        throw DependencyCycleException(this :: stack.takeWhile(_ != this))
      }
    }

  private def doInit(implicit ec: ExecutionContext): Future[T] = try {
    val promise = Promise[T]()
    if (savedFuture.compareAndSet(null, promise.future)) {
      val resultFuture = Future.traverse(dependencies)(_.doInit).map(deps => create(deps.toIndexedSeq))
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

trait Injectable extends Any
object Injectable {
  @compileTimeOnly("implicit Component[T] => implicit T inference only works inside argument to Component(...) macro")
  implicit def inject[T <: Injectable](implicit component: Component[T]): T = sys.error("stub")
}
