package com.avsystem.commons
package di

import com.avsystem.commons.macros.di.ComponentMacros
import com.avsystem.commons.misc.SourceInfo

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference
import scala.annotation.compileTimeOnly

/** Base trait for classes that define collections of interdependent [[Component]]s.
  */
trait Components extends ComponentsLowPrio {
  protected def componentNamePrefix: String = ""

  protected def componentInfo(sourceInfo: SourceInfo): ComponentInfo =
    ComponentInfo(componentNamePrefix, sourceInfo)

  /** Creates a [[Component]] based on a definition (i.e. a constructor invocation). The definition may refer to other
    * components as dependencies using `.ref`. This macro will transform the definition by extracting dependencies in a
    * way that allows them to be initialized in parallel, before initializing the current component itself.
    */
  protected def component[T](definition: => T)(implicit sourceInfo: SourceInfo): Component[T] =
    macro ComponentMacros.component[T]

  /** Asynchronous version of [[component]] macro.
    */
  protected def asyncComponent[T](definition: ExecutionContext => Future[T])(implicit sourceInfo: SourceInfo)
    : Component[T] = macro ComponentMacros.asyncComponent[T]

  /** This is the same as [[component]] except that the created [[Component]] is cached inside an outer instance that
    * implements [[Components]]. This way you can implement your components using `def`s rather than `val`s (`val`s can
    * be problematic in traits) but caching will make sure that your `def` always returns the same, cached [[Component]]
    * instance. The cache key is based on source position so overriding a method that returns `singleton` will create
    * separate [[Component]] with different cache key.
    */
  protected def singleton[T](definition: => T)(implicit sourceInfo: SourceInfo): Component[T] =
    macro ComponentMacros.singleton[T]

  /** Asynchronous version of [[singleton]] macro.
    */
  protected def asyncSingleton[T](definition: ExecutionContext => Future[T])(implicit sourceInfo: SourceInfo)
    : Component[T] = macro ComponentMacros.asyncSingleton[T]

  private lazy val singletonsCache = new ConcurrentHashMap[ComponentInfo, AtomicReference[Future[_]]]

  protected def cached[T](component: Component[T], freshInfo: ComponentInfo): Component[T] = {
    val cacheStorage =
      singletonsCache.computeIfAbsent(freshInfo, _ => new AtomicReference).asInstanceOf[AtomicReference[Future[T]]]
    component.cached(cacheStorage, freshInfo)
  }

  protected def reifyAllSingletons: List[Component[_]] = macro ComponentMacros.reifyAllSingletons

  // avoids divergent implicit expansion involving `inject`
  // this is not strictly necessary but makes compiler error messages nicer
  // i.e. the compiler will emit "could not find implicit value" instead of "divergent implicit expansion"
  implicit def ambiguousArbitraryComponent1[T]: Component[T] = null
  implicit def ambiguousArbitraryComponent2[T]: Component[T] = null

  implicit def autoComponent[T](definition: => T)(implicit sourceInfo: SourceInfo): AutoComponent[T] =
    macro ComponentMacros.autoComponent[T]

  protected def optEmptyComponent: Component[Opt[Nothing]] =
    singleton(Opt.Empty)

  protected def noneComponent: Component[Option[Nothing]] =
    singleton(None)

  protected def sequenceOpt[T](componentOpt: Opt[Component[T]]): Component[Opt[T]] =
    componentOpt.mapOr(optEmptyComponent, c => component(c.ref.opt))

  protected def sequenceOption[T](componentOpt: Option[Component[T]]): Component[Option[T]] =
    componentOpt.mapOr(noneComponent, c => component(c.ref.option))
}
trait ComponentsLowPrio {
  @compileTimeOnly(
    "implicit Component[T] => implicit T inference only works inside code passed to component/singleton macro"
  )
  implicit def inject[T](implicit component: Component[T]): T = sys.error("stub")
}
