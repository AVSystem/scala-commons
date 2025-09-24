package com.avsystem.commons
package di

trait InitializingComponent {
  def init(): Unit
  final def initialized(): this.type = init().thenReturn(this)
}

trait DisposableComponent {
  def destroy(): Unit
}

trait AsyncInitializingComponent {
  def init()(implicit ec: ExecutionContext): Future[Unit]
  final def initialized()(implicit ec: ExecutionContext): Future[this.type] = init()(using ec).map[this.type](_ => this)(using ec)
}

trait AsyncDisposableComponent {
  def destroy()(implicit ec: ExecutionContext): Future[Unit]
}
