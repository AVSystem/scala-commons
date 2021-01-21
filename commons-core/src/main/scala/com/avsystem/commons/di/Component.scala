package com.avsystem.commons
package di

import com.avsystem.commons.di.Component.DfsPtr
import com.avsystem.commons.macros.di.ComponentMacros
import com.avsystem.commons.misc.SourceInfo

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference
import scala.annotation.{compileTimeOnly, tailrec}

case class ComponentInitializationException(component: Component[_], cause: Throwable)
  extends Exception(s"failed to initialize component ${component.info}", cause)

case class DependencyCycleException(cyclePath: List[Component[_]])
  extends Exception(s"component dependency cycle detected:\n${cyclePath.iterator.map(_.info).map("  " + _).mkString(" ->\n")}")

/**
  * Represents a lazily initialized component in a dependency injection setting. The name "component" indicates
  * that the value is often an application building block like a database service, data access object, HTTP server etc.
  * which is associated with some side-effectful initialization code.
  * However, [[Component]] can hold values of any type.
  *
  * You can think of [[Component]] as a high-level `lazy val` with more features:
  * parallel initialization of dependencies, dependency cycle detection, source code awareness.
  */
final class Component[+T](
  val sourceInfo: SourceInfo,
  deps: => IndexedSeq[Component[_]],
  create: IndexedSeq[Any] => T,
  cachedStorage: Opt[AtomicReference[Future[T]]] = Opt.Empty,
) {

  def name: String = sourceInfo.enclosingSymbols.head
  def info: String = s"$name(${sourceInfo.fileName}:${sourceInfo.line})"
  def isCached: Boolean = cachedStorage.isDefined

  /**
    * Returns dependencies of this component extracted from the component definition.
    * You can use this to inspect the dependency graph without initializing any components.
    */
  lazy val dependencies: IndexedSeq[Component[_]] = deps

  private[this] val storage: AtomicReference[Future[T]] =
    cachedStorage.getOrElse(new AtomicReference)

  private def sameStorage(otherStorage: AtomicReference[_]): Boolean =
    storage eq otherStorage

  // equality based on storage identity is important for cycle detection with cached components
  override def hashCode(): Int = storage.hashCode()
  override def equals(obj: Any): Boolean = obj match {
    case c: Component[_] => c.sameStorage(storage)
    case _ => false
  }

  /**
    * Phantom method that indicates an asynchronous reference to this component inside definition of some other component.
    * This method is rewritten in compile time by [[Components.component]] or [[Components.singleton]] macro.
    * The component being referred is extracted as a dependency and initialized before the component that refers to
    * it. This way multiple dependencies can be initialized in parallel.
    *
    * @example
    * {{{
    *   class FooService
    *   class BarService
    *   class Application(foo: FooService, bar: BarService)
    *
    *   object MyComponents extends Components {
    *     def foo: Component[FooService] = singleton(new FooService)
    *     def bar: Component[BarService] = singleton(new BarService)
    *
    *     // before `app` is initialized, `foo` and `bar` can be initialized in parallel
    *     def app: Component[Application] = singleton(new Application(foo.ref, bar.ref))
    *   }
    * }}}
    */
  @compileTimeOnly(".ref can only be used inside code passed to component/singleton(...) macro")
  def ref: T = sys.error("stub")

  def getIfReady: Opt[T] =
    storage.get.opt.flatMap(_.value.map(_.get).toOpt)

  def dependsOn(moreDeps: Component[_]*): Component[T] =
    new Component(sourceInfo, deps ++ moreDeps, create, cachedStorage)

  private[di] def cached[T0 >: T](cachedStorage: AtomicReference[Future[T0]])(implicit sourceInfo: SourceInfo): Component[T0] =
    new Component(sourceInfo, deps, create, Opt(cachedStorage))

  /**
    * Validates this component by checking its dependency graph for cycles.
    * A [[DependencyCycleException]] is thrown when a cycle is detected.
    */
  def validate(): Unit = {
    val visited = new MHashMap[Component[_], Boolean]
    visited(this) = false
    detectCycles(List(DfsPtr(this, dependencies.toList)), visited)
  }

  @tailrec // DFS
  private def detectCycles(stack: List[DfsPtr], visited: MHashMap[Component[_], Boolean]): Unit =
    stack match {
      case DfsPtr(component, deps) :: stackTail => deps match {
        case Nil =>
          visited(component) = true
          detectCycles(stackTail, visited)
        case nextDep :: depsTail => visited.get(nextDep) match {
          case None =>
            visited(nextDep) = false
            detectCycles(DfsPtr(nextDep, nextDep.dependencies.toList) :: DfsPtr(component, depsTail) :: stackTail, visited)
          case Some(true) => // already visited, do nothing
          case Some(false) => // cycle
            val cyclePath = nextDep :: (nextDep :: stack.map(_.component).takeWhile(_ != nextDep)).reverse
            throw DependencyCycleException(cyclePath)
        }
      }
      case Nil =>
    }

  /**
    * Forces initialization of this component and its dependencies (in parallel, using given `ExecutionContext`).
    * Returns a `Future` containing the initialized component value.
    * NOTE: the component is initialized only once and its value is cached.
    */
  def init(implicit ec: ExecutionContext): Future[T] =
    doInit(Nil, starting = true)

  private def doInit(stack: List[Component[_]], starting: Boolean)(implicit ec: ExecutionContext): Future[T] = {
    val promise = Promise[T]()
    if (storage.compareAndSet(null, promise.future)) {
      if (starting) {
        validate()
      }
      val newStack = this :: stack
      val resultFuture =
        Future.traverse(dependencies)(_.doInit(newStack, starting = false))
          .mapNow(create)
          .recoverNow {
            case NonFatal(cause) =>
              throw ComponentInitializationException(this, cause)
          }
      promise.completeWith(resultFuture)
    }
    storage.get()
  }
}
object Component {
  sealed abstract class CreateResult[+T]
  object CreateResult {
    final case class Ready[+T](value: T) extends CreateResult[T]
    final case class More[+T](nextStep: Component[T]) extends CreateResult[T]
  }

  private case class DfsPtr(component: Component[_], deps: List[Component[_]])
}

trait Components extends ComponentsLowPrio {
  /**
    * Creates a [[Component]] based on a definition (i.e. a constructor invocation). The definition may refer to
    * other components as dependencies using `.ref`. This macro will transform the definition by extracting dependencies
    * in a way that allows them to be initialized in parallel, before initializing the current component itself.
    */
  protected[this] def component[T](definition: => T)(implicit sourceInfo: SourceInfo): Component[T] = macro ComponentMacros.prototype[T]

  /**
    * This is the same as [[component]] except that the created [[Component]] is cached inside an outer instance that
    * implements [[Components]]. This way you can implement your components using `def`s rather than `val`s
    * (`val`s can be problematic in traits) but caching will make sure that your `def` always returns the same,
    * cached [[Component]] instance. The cache key is based on source position so overriding a method that returns
    * `singleton` will create separate [[Component]] with different cache key.
    */
  protected[this] def singleton[T](definition: => T)(implicit sourceInfo: SourceInfo): Component[T] = macro ComponentMacros.singleton[T]

  private[this] val singletonsCache = new ConcurrentHashMap[SourceInfo, AtomicReference[Future[_]]]

  protected[this] def cached[T](component: Component[T])(implicit sourceInfo: SourceInfo): Component[T] = {
    val cacheStorage = singletonsCache
      .computeIfAbsent(sourceInfo, _ => new AtomicReference)
      .asInstanceOf[AtomicReference[Future[T]]]
    component.cached(cacheStorage)
  }

  protected[this] def reifyAllSingletons: List[Component[_]] = macro ComponentMacros.reifyAllSingletons

  // avoids divergent implicit expansion involving `inject`
  // this is not strictly necessary but makes compiler error messages nicer
  // i.e. the compiler will emit "could not find implicit value" instead of "divergent implicit expansion"
  implicit def ambiguousArbitraryComponent1[T]: Component[T] = null
  implicit def ambiguousArbitraryComponent2[T]: Component[T] = null
}
trait ComponentsLowPrio {
  @compileTimeOnly("implicit Component[T] => implicit T inference only works inside code passed to component/singleton macro")
  implicit def inject[T](implicit component: Component[T]): T = sys.error("stub")
}
