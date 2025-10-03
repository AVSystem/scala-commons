package com.avsystem.commons
package di

import monix.execution.atomic.{Atomic, AtomicInt}
import org.scalatest.funsuite.AsyncFunSuite

final class LifeCycleComponentTest extends AsyncFunSuite with Components {

  object init {
    val inits: AtomicInt = Atomic(0)
    val component: Component[InitializingComponent] = singleton {
      new InitializingComponent {
        override def init(): Unit = inits += 1
      }
    }
  }

  object asyncInit {
    val inits: AtomicInt = Atomic(0)
    val component: Component[AsyncInitializingComponent] = singleton {
      new AsyncInitializingComponent {
        def init()(implicit ec: ExecutionContext): Future[Unit] = Future {
          inits += 1
        }(ec)
      }
    }
  }

  object disposable {
    val destroys: AtomicInt = Atomic(0)
    val component: Component[DisposableComponent] = singleton {
      new DisposableComponent {
        def destroy(): Unit = destroys += 1
      }
    }
  }


  object asyncDisposable {
    val destroys: AtomicInt = Atomic(0)
    val component: Component[AsyncDisposableComponent] = singleton {
      new AsyncDisposableComponent {
        def destroy()(implicit ec: ExecutionContext): Future[Unit] = Future {
          destroys += 1
        }(using ec)
      }
    }
  }

  test("InitializingComponent singleton initializes once and returns same instance")(for {
    _ <- Future.unit
    _ = assert(init.inits.get() == 0)
    c1 <- init.component.init
    _ = assert(init.inits.get() == 1)
    c2 <- init.component.init
    _ = assert(init.inits.get() == 1)
  } yield assert(c1 eq c2))

  test("AsyncInitializingComponent singleton initializes once and returns same instance")(for {
    _ <- Future.unit
    _ = assert(asyncInit.inits.get() == 0)
    c1 <- asyncInit.component.init
    _ = assert(asyncInit.inits.get() == 1)
    c2 <- asyncInit.component.init
    _ = assert(asyncInit.inits.get() == 1)
  } yield assert(c1 eq c2))

  test("DisposableComponent destroy triggers side effect")(for {
    _ <- Future.unit
    _ <- disposable.component.destroy
    _ = assert(disposable.destroys.get() == 0)
    _ <- disposable.component.init
    _ = assert(disposable.destroys.get() == 0)
    _ <- disposable.component.destroy
    _ = assert(disposable.destroys.get() == 1)
    _ <- disposable.component.destroy
    _ = assert(disposable.destroys.get() == 1)
  } yield succeed)

  test("AsyncDisposableComponent destroy triggers side effect")(for {
    _ <- Future.unit
    _ <- asyncDisposable.component.destroy
    _ = assert(asyncDisposable.destroys.get() == 0)
    _ <- asyncDisposable.component.init
    _ = assert(asyncDisposable.destroys.get() == 0)
    _ <- asyncDisposable.component.destroy
    _ = assert(asyncDisposable.destroys.get() == 1)
    _ <- asyncDisposable.component.destroy
    _ = assert(asyncDisposable.destroys.get() == 1)
  } yield succeed)
}
