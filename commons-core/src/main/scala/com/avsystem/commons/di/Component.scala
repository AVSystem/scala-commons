package com.avsystem.commons
package di

import com.avsystem.commons.di.Component.DestroyFunction
import com.avsystem.commons.macros.di.ComponentMacros
import com.avsystem.commons.misc.{GraphUtils, SourceInfo}

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference
import scala.annotation.compileTimeOnly
import scala.annotation.unchecked.uncheckedVariance

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

  @compileTimeOnly("implicit ComponentInfo is only available inside code passed to component/singleton macro")
  implicit def info: ComponentInfo = sys.error("stub")
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
  creator: IndexedSeq[Any] => ExecutionContext => Future[T],
  destroyer: DestroyFunction[T] = Component.emptyDestroy,
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
    new Component(info, deps ++ moreDeps, creator, destroyer, cachedStorage)

  def asyncDestroyWith(destroyFun: DestroyFunction[T]): Component[T] = {
    val newDestroyer: DestroyFunction[T] =
      implicit ctx => t => destroyer(ctx)(t).flatMap(_ => destroyFun(ctx)(t))
    new Component(info, deps, creator, newDestroyer, cachedStorage)
  }

  def destroyWith(destroyFun: T => Unit): Component[T] =
    asyncDestroyWith(implicit ctx => t => Future(destroyFun(t)))

  private[di] def cached(cachedStorage: AtomicReference[Future[T@uncheckedVariance]], info: ComponentInfo): Component[T] =
    new Component(info, deps, creator, destroyer, Opt(cachedStorage))

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
    doInit(starting = true)

  /**
    * Destroys this component and all its dependencies (in reverse initialization order, i.e. first the component
    * and then its dependencies. Destroying calls the function that was registered with [[destroyWith]] or
    * [[asyncDestroyWith]] and clears the cached component instance so that it is created anew if [[init]] is called
    * again.
    * If possible, independent components are destroyed in parallel, using given `ExecutionContext`.
    */
  def destroy(implicit ec: ExecutionContext): Future[Unit] =
    Component.destroyAll(List(this))

  private def doDestroy(implicit ec: ExecutionContext): Future[Unit] =
    getIfReady.fold(Future.unit) { value =>
      storage.set(null)
      destroyer(ec)(value)
    }

  private def doInit(starting: Boolean)(implicit ec: ExecutionContext): Future[T] =
    storage.getPlain match {
      case null =>
        val promise = Promise[T]()
        if (storage.compareAndSet(null, promise.future)) {
          if (starting) {
            validate()
          }
          val resultFuture =
            Future.traverse(dependencies)(_.doInit(starting = false))
              .flatMap(resolvedDeps => creator(resolvedDeps)(ec))
              .recoverNow {
                case NonFatal(cause) =>
                  throw ComponentInitializationException(this, cause)
              }
          promise.completeWith(resultFuture)
        }
        storage.get()
      case future =>
        future
    }
}
object Component {
  type DestroyFunction[-T] = ExecutionContext => T => Future[Unit]

  def emptyDestroy[T]: DestroyFunction[T] =
    reusableEmptyDestroy.asInstanceOf[DestroyFunction[T]]

  private[this] val reusableEmptyDestroy: DestroyFunction[Any] =
    _ => _ => Future.unit

  def async[T](definition: => T): ExecutionContext => Future[T] =
    implicit ctx => Future(definition)

  private case class DfsPtr(component: Component[_], deps: List[Component[_]])

  def validateAll(components: Seq[Component[_]]): Unit =
    GraphUtils.dfs(components)(
      _.dependencies.toList,
      onCycle = (node, stack) => {
        val cyclePath = node :: (node :: stack.map(_.node).takeWhile(_ != node)).reverse
        throw DependencyCycleException(cyclePath)
      }
    )

  /**
    * Destroys all given components and their dependencies by calling their destroy function (registered with
    * [[Component.destroyWith()]] or [[Component.asyncDestroyWith()]]) and clearing up cached component instances.
    * It is ensured that a component is only destroyed after all components that depend on it are destroyed
    * (reverse initialization order).
    * Independent components are destroyed in parallel, using given `ExecutionContext`.
    */
  def destroyAll(components: Seq[Component[_]])(implicit ec: ExecutionContext): Future[Unit] = {
    val reverseGraph = new MHashMap[Component[_], MListBuffer[Component[_]]]
    val terminals = new MHashSet[Component[_]]
    GraphUtils.dfs(components)(
      _.dependencies.toList,
      onEnter = { (c, _) =>
        reverseGraph.getOrElseUpdate(c, new MListBuffer) // make sure there is entry for all nodes
        if (c.dependencies.nonEmpty)
          c.dependencies.foreach { dep =>
            reverseGraph.getOrElseUpdate(dep, new MListBuffer) += c
          }
        else
          terminals += c
      }
    )
    val destroyFutures = new MHashMap[Component[_], Future[Unit]]

    def doDestroy(c: Component[_]): Future[Unit] =
      destroyFutures.getOrElseUpdate(c, Future.traverse(reverseGraph(c))(doDestroy).flatMap(_ => c.doDestroy))

    Future.traverse(reverseGraph.keys)(doDestroy).toUnit
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

/**
  * Base trait for classes that define collections of interdependent [[Component]]s.
  */
trait Components extends ComponentsLowPrio {
  protected def componentNamePrefix: String = ""

  protected def componentInfo(sourceInfo: SourceInfo): ComponentInfo =
    ComponentInfo(componentNamePrefix, sourceInfo)

  /**
    * Creates a [[Component]] based on a definition (i.e. a constructor invocation). The definition may refer to
    * other components as dependencies using `.ref`. This macro will transform the definition by extracting dependencies
    * in a way that allows them to be initialized in parallel, before initializing the current component itself.
    */
  protected def component[T](definition: => T)(implicit sourceInfo: SourceInfo): Component[T] = macro ComponentMacros.component[T]

  /**
    * Asynchronous version of [[component]] macro.
    */
  protected def asyncComponent[T](definition: ExecutionContext => Future[T])(implicit sourceInfo: SourceInfo): Component[T] = macro ComponentMacros.asyncComponent[T]

  /**
    * This is the same as [[component]] except that the created [[Component]] is cached inside an outer instance that
    * implements [[Components]]. This way you can implement your components using `def`s rather than `val`s
    * (`val`s can be problematic in traits) but caching will make sure that your `def` always returns the same,
    * cached [[Component]] instance. The cache key is based on source position so overriding a method that returns
    * `singleton` will create separate [[Component]] with different cache key.
    */
  protected def singleton[T](definition: => T)(implicit sourceInfo: SourceInfo): Component[T] = macro ComponentMacros.singleton[T]

  /**
    * Asynchronous version of [[singleton]] macro.
    */
  protected def asyncSingleton[T](definition: ExecutionContext => Future[T])(implicit sourceInfo: SourceInfo): Component[T] = macro ComponentMacros.asyncSingleton[T]

  private lazy val singletonsCache = new ConcurrentHashMap[ComponentInfo, AtomicReference[Future[_]]]

  protected def cached[T](component: Component[T], freshInfo: ComponentInfo): Component[T] = {
    val cacheStorage = singletonsCache
      .computeIfAbsent(freshInfo, _ => new AtomicReference)
      .asInstanceOf[AtomicReference[Future[T]]]
    component.cached(cacheStorage, freshInfo)
  }

  protected def reifyAllSingletons: List[Component[_]] = macro ComponentMacros.reifyAllSingletons

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
