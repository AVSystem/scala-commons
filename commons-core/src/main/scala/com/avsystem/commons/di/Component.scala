package com.avsystem.commons
package di

import com.avsystem.commons.concurrent.RunInQueueEC
import com.avsystem.commons.macros.di.ComponentMacros
import com.avsystem.commons.misc.SourceInfo

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference
import scala.annotation.compileTimeOnly
import scala.concurrent.Await
import scala.concurrent.duration.Duration

case class ComponentName(name: String) extends AnyVal {
  override def toString: String = name
}
object ComponentName {
  @compileTimeOnly("implicit ComponentName is only available inside code passed to component(...) macro")
  implicit def componentName: ComponentName = sys.error("stub")
}

case class ComponentInitializationException(component: Component[_], cause: Throwable)
  extends Exception(s"failed to initialize component ${component.info}", cause)

case class DependencyCycleException(cyclePath: List[Component[_]])
  extends Exception(s"component dependency cycle detected: ${cyclePath.map(_.info).mkString("", " -> ", " -> " + cyclePath.head.info)}")

abstract class Component[+T] {
  def name: String
  def sourceInfo: SourceInfo

  def cacheKey: String = s"${sourceInfo.filePath}:${sourceInfo.offset}"
  def info: String = s"$name(${sourceInfo.fileName}:${sourceInfo.line})"

  private[this] val savedFuture = new AtomicReference[Future[T]]

  @compileTimeOnly(".ref can only be used inside code passed to component(...) macro")
  def ref: T = sys.error("stub")

  def dependsOn(deps: Component[_]*): Component[T] =
    new Component.WithAdditionalDeps(this, deps)

  final def init(): T =
    Await.result(parallelInit()(RunInQueueEC), Duration.Inf)

  final def parallelInit()(implicit ec: ExecutionContext): Future[T] =
    doParallelInit(cycleCheck = true).catchFailures

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

  private def doParallelInit(cycleCheck: Boolean)(implicit ec: ExecutionContext): Future[T] = {
    val promise = Promise[T]()
    if (savedFuture.compareAndSet(null, promise.future)) {
      if (cycleCheck) {
        detectCycles(Nil, new MHashSet)
      }
      val resultFuture =
        Future.traverse(dependencies)(_.doParallelInit(cycleCheck = false))
          .map(deps => create(deps.toIndexedSeq))
          .recoverNow {
            case NonFatal(cause) => throw ComponentInitializationException(this, cause)
          }
      promise.completeWith(resultFuture)
    }
    savedFuture.get()
  }

  def dependencies: IndexedSeq[Component[_]]

  protected def create(resolvedDeps: IndexedSeq[Any]): T
}
object Component {
  private class WithAdditionalDeps[T](wrapped: Component[T], deps: Seq[Component[_]]) extends Component[T] {
    def name: String = wrapped.name
    def sourceInfo: SourceInfo = wrapped.sourceInfo
    def dependencies: IndexedSeq[Component[_]] = wrapped.dependencies ++ deps
    protected def create(resolvedDeps: IndexedSeq[Any]): T = wrapped.create(resolvedDeps)
  }
}

trait Components extends ComponentsLowPrio {
  def component[T](definition: => T): Component[T] = macro ComponentMacros.componentCreate[T]
  def cachedComponent[T](definition: => T): Component[T] = macro ComponentMacros.cachedComponentCreate[T]

  private val componentsCache = new ConcurrentHashMap[String, Component[_]]

  protected def cacheComponent[T](component: Component[T]): Component[T] =
    componentsCache.computeIfAbsent(component.cacheKey, _ => component).asInstanceOf[Component[T]]

  // avoids divergent implicit expansion involving `inject`
  // this is not strictly necessary but makes compiler error messages nicer
  // i.e. the compiler will emit "could not find implicit value" instead of "divergent implicit expansion"
  implicit def ambiguousArbitraryComponent1[T]: Component[T] = null
  implicit def ambiguousArbitraryComponent2[T]: Component[T] = null
}
trait ComponentsLowPrio {
  @compileTimeOnly("implicit Component[T] => implicit T inference only works inside code passed to component(...) macro")
  implicit def inject[T](implicit component: Component[T]): T = sys.error("stub")
}
