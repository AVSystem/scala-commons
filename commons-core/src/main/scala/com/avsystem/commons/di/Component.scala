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
  extends Exception(s"component dependency cycle detected:\n${cyclePath.iterator.map(_.info).map("  " + _).mkString(" ->\n")}")

abstract class Component[+T] {

  import Component._

  def name: String
  def sourceInfo: SourceInfo

  def cacheKey: String = s"${sourceInfo.filePath}:${sourceInfo.offset}"
  def info: String = s"$name(${sourceInfo.fileName}:${sourceInfo.line})"

  private[this] val savedFuture = new AtomicReference[Future[T]]

  @compileTimeOnly(".ref can only be used inside code passed to component(...) macro")
  def ref: T = sys.error("stub")

  final def get(implicit ec: ExecutionContext): T =
    Await.result(init, Duration.Inf)

  final def getNow: T =
    get(RunInQueueEC)

  final def init(implicit ec: ExecutionContext): Future[T] =
    doInit(Nil).catchFailures

  private def doInit(stack: List[Component[_]])(implicit ec: ExecutionContext): Future[T] = {
    if (stack.contains(this)) {
      val cyclePath = this :: (this :: stack.takeWhile(_ != this)).reverse
      throw DependencyCycleException(cyclePath)
    }
    val promise = Promise[T]()
    if (savedFuture.compareAndSet(null, promise.future)) {
      val newStack = this :: stack
      val resultFuture =
        Future.traverse(dependencies)(_.doInit(newStack))
          .flatMapNow(deps => create(deps) match {
            case CreateResult.Ready(value) =>
              Future.successful(value)
            case CreateResult.More(nextComponent) =>
              nextComponent.doInit(newStack)
          })
          .recoverNow {
            case NonFatal(cause) =>
              throw ComponentInitializationException(this, cause)
          }
      promise.completeWith(resultFuture)
    }
    savedFuture.get()
  }

  protected def dependencies: IndexedSeq[Component[_]]
  protected def create(resolvedDeps: IndexedSeq[Any]): CreateResult[T]
}
object Component {
  sealed abstract class CreateResult[+T]
  object CreateResult {
    final case class Ready[+T](value: T) extends CreateResult[T]
    final case class More[+T](nextStep: Component[T]) extends CreateResult[T]
  }
}

final class ComponentImpl[T](
  val name: String,
  val sourceInfo: SourceInfo,
  deps: => IndexedSeq[Component[_]],
  createFun: IndexedSeq[Any] => Component.CreateResult[T]
) extends Component[T] {
  protected lazy val dependencies: IndexedSeq[Component[_]] = deps
  protected def create(resolvedDeps: IndexedSeq[Any]): Component.CreateResult[T] = createFun(resolvedDeps)
}

trait Components extends ComponentsLowPrio {
  def component[T](definition: => T): Component[T] = macro ComponentMacros.componentCreate[T]
  def cachedComponent[T](definition: => T): Component[T] = macro ComponentMacros.cachedComponentCreate[T]

  private val componentsCache = new ConcurrentHashMap[String, Component[_]]

  def cached[T](component: Component[T]): Component[T] =
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
