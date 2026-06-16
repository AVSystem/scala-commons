package com.avsystem.commons
package di

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

  // TODO[scala3-port]: component (Scala 2 macro def) (L)
  protected def component[T](definition: => T)(implicit sourceInfo: SourceInfo): Component[T] = ???

  // TODO[scala3-port]: asyncComponent (Scala 2 macro def) (L)
  protected def asyncComponent[T](definition: ExecutionContext => Future[T])(implicit sourceInfo: SourceInfo)
    : Component[T] = ???

  // TODO[scala3-port]: singleton (Scala 2 macro def) (L)
  protected def singleton[T](definition: => T)(implicit sourceInfo: SourceInfo): Component[T] = ???

  // TODO[scala3-port]: asyncSingleton (Scala 2 macro def) (L)
  protected def asyncSingleton[T](definition: ExecutionContext => Future[T])(implicit sourceInfo: SourceInfo)
    : Component[T] = ???

  private lazy val singletonsCache = new ConcurrentHashMap[ComponentInfo, AtomicReference[Future[?]]]

  protected def cached[T](component: Component[T], freshInfo: ComponentInfo): Component[T] = {
    val cacheStorage =
      singletonsCache.computeIfAbsent(freshInfo, _ => new AtomicReference).asInstanceOf[AtomicReference[Future[T]]]
    component.cached(cacheStorage, freshInfo)
  }

  // TODO[scala3-port]: reifyAllSingletons (Scala 2 macro def) (L)
  protected def reifyAllSingletons: List[Component[?]] = ???

  // avoids divergent implicit expansion involving `inject`
  // this is not strictly necessary but makes compiler error messages nicer
  // i.e. the compiler will emit "could not find implicit value" instead of "divergent implicit expansion"
  implicit def ambiguousArbitraryComponent1[T]: Component[T] = null
  implicit def ambiguousArbitraryComponent2[T]: Component[T] = null

  // TODO[scala3-port]: autoComponent (Scala 2 macro def) (L)
  implicit def autoComponent[T](definition: => T)(implicit sourceInfo: SourceInfo): AutoComponent[T] = ???

  // TODO[scala3-port]: optEmptyComponent (depends on stubbed singleton macro) (S)
  protected def optEmptyComponent: Component[Opt[Nothing]] = ???

  // TODO[scala3-port]: noneComponent (depends on stubbed singleton macro) (S)
  protected def noneComponent: Component[Option[Nothing]] = ???

  // TODO[scala3-port]: sequenceOpt (depends on stubbed component macro) (S)
  protected def sequenceOpt[T](componentOpt: Opt[Component[T]]): Component[Opt[T]] = ???

  // TODO[scala3-port]: sequenceOption (depends on stubbed component macro) (S)
  protected def sequenceOption[T](componentOpt: Option[Component[T]]): Component[Option[T]] = ???
}
trait ComponentsLowPrio {
  @compileTimeOnly(
    "implicit Component[T] => implicit T inference only works inside code passed to component/singleton macro"
  )
  implicit def inject[T](implicit component: Component[T]): T = sys.error("stub")
}
