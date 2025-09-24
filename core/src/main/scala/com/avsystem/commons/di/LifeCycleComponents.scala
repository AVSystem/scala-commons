package com.avsystem.commons
package di

trait InitializingComponent {
  def init(): Unit
}

trait DisposableComponent {
  def destroy(): Unit
}

trait AsyncInitializingComponent {
  def init()(implicit ec: ExecutionContext): Future[Unit]
}

trait AsyncDisposableComponent {
  def destroy()(implicit ec: ExecutionContext): Future[Unit]
}
