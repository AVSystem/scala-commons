package com.avsystem.commons
package di

import com.avsystem.commons.macros.di.ComponentMacros
import com.avsystem.commons.misc.SourceInfo

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference
import scala.annotation.{compileTimeOnly, tailrec}

case class ComponentInitializationException(component: Component[_], cause: Throwable)
  extends Exception(s"failed to initialize component ${component.info}", cause)

case class DependencyCycleException(cyclePath: List[Component[_]])
  extends Exception(s"component dependency cycle detected:\n${cyclePath.iterator.map(_.info).map("  " + _).mkString(" ->\n")}")

case class ComponentInfo(
  name: String,
  filePath: String,
  fileName: String,
  lineNumber: Int
) {
  override def toString: String = s"$name($fileName:$lineNumber)"
}
object ComponentInfo {
  def apply(namePrefix: String, sourceInfo: SourceInfo): ComponentInfo =
    new ComponentInfo(namePrefix + sourceInfo.enclosingSymbols.head, sourceInfo.filePath, sourceInfo.fileName, sourceInfo.line)
}

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
  val info: ComponentInfo,
  deps: => IndexedSeq[Component[_]],
  create: IndexedSeq[Any] => T,
  cachedStorage: Opt[AtomicReference[Future[T]]] = Opt.Empty,
) {

  def name: String = info.name
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

  def getIfReady: Option[T] =
    storage.get.option.flatMap(_.value.map(_.get))

  def dependsOn(moreDeps: Component[_]*): Component[T] =
    new Component(info, deps ++ moreDeps, create, cachedStorage)

  private[di] def cached[T0 >: T](cachedStorage: AtomicReference[Future[T0]], info: ComponentInfo): Component[T0] =
    new Component(info, deps, create, Opt(cachedStorage))

  /**
    * Validates this component by checking its dependency graph for cycles.
    * A [[DependencyCycleException]] is thrown when a cycle is detected.
    */
  def validate(): Unit =
    Component.validateAll(List(this))

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
          .map(create)
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
  private case class DfsPtr(component: Component[_], deps: List[Component[_]])

  def validateAll(components: Seq[Component[_]]): Unit = {
    val visited = new MHashMap[Component[_], Boolean]
    components.foreach { component =>
      if (!visited.contains(component)) {
        visited(component) = false
        detectCycles(List(DfsPtr(component, component.dependencies.toList)), visited)
      }
    }
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
          case Some(true) =>
            detectCycles(DfsPtr(component, depsTail) :: stackTail, visited)
          case Some(false) => // cycle
            val cyclePath = nextDep :: (nextDep :: stack.map(_.component).takeWhile(_ != nextDep)).reverse
            throw DependencyCycleException(cyclePath)
        }
      }
      case Nil =>
    }
}

/**
  * A wrapper over [[Component]] that has an implicit conversion from arbitrary expression
  * of type T to [[AutoComponent]]. This is used when you need to accept a parameter that may contain other
  * component references.
  *
  * Using [[AutoComponent]] avoids explicit wrapping of expressions passed as that parameter
  * into [[Component]] (using `component` macro).
  */
case class AutoComponent[+T](component: Component[T]) extends AnyVal

trait Components extends ComponentsLowPrio {
  protected def componentNamePrefix: String = ""

  protected[this] def componentInfo(sourceInfo: SourceInfo): ComponentInfo =
    ComponentInfo(componentNamePrefix, sourceInfo)

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

  private[this] val singletonsCache = new ConcurrentHashMap[ComponentInfo, AtomicReference[Future[_]]]

  protected[this] def cached[T](component: Component[T], freshInfo: ComponentInfo): Component[T] = {
    val cacheStorage = singletonsCache
      .computeIfAbsent(freshInfo, _ => new AtomicReference)
      .asInstanceOf[AtomicReference[Future[T]]]
    component.cached(cacheStorage, freshInfo)
  }

  protected[this] def reifyAllSingletons: List[Component[_]] = macro ComponentMacros.reifyAllSingletons

  // avoids divergent implicit expansion involving `inject`
  // this is not strictly necessary but makes compiler error messages nicer
  // i.e. the compiler will emit "could not find implicit value" instead of "divergent implicit expansion"
  implicit def ambiguousArbitraryComponent1[T]: Component[T] = null
  implicit def ambiguousArbitraryComponent2[T]: Component[T] = null

  implicit def autoComponent[T](definition: => T)(implicit sourceInfo: SourceInfo): AutoComponent[T] = macro ComponentMacros.autoComponent[T]
}
trait ComponentsLowPrio {
  @compileTimeOnly("implicit Component[T] => implicit T inference only works inside code passed to component/singleton macro")
  implicit def inject[T](implicit component: Component[T]): T = sys.error("stub")
}
