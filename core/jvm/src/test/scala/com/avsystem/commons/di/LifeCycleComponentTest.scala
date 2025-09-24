package com.avsystem.commons
package di

import monix.execution.atomic.{Atomic, AtomicBoolean, AtomicInt}
import org.scalatest.funsuite.AsyncFunSuite

class LifeCycleComponentTest extends AsyncFunSuite with Components {

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
    val destroyed: AtomicBoolean = Atomic(false)
    val component: Component[DisposableComponent] = singleton {
      new DisposableComponent {
        def destroy(): Unit = destroyed.set(true)
      }
    }
  }


  object asyncDisposable {
    val destroyed: AtomicBoolean = Atomic(false)
    val component: Component[AsyncDisposableComponent] = singleton {
      new AsyncDisposableComponent {
        def destroy()(implicit ec: ExecutionContext): Future[Unit] = Future {
          destroyed.set(true)
        }(ec)
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

  test("DisposableComponent destroy triggers side effect") {
    assert(!disposable.destroyed.get())
    disposable.component.destroy.map { _ =>
      assert(disposable.destroyed.get())
    }
  }

  test("AsyncDisposableComponent destroy triggers side effect") {
    assert(!asyncDisposable.destroyed.get())
    asyncDisposable.component.destroy.map { _ =>
      assert(asyncDisposable.destroyed.get())
    }
  }
}