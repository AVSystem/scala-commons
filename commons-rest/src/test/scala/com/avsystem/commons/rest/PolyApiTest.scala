package com.avsystem.commons
package rest

import com.avsystem.commons.concurrent.Async
import com.avsystem.commons.meta.MacroInstances
import com.avsystem.commons.rest.openapi.{OpenApiMetadata, RestResponses, RestResultType}
import com.avsystem.commons.rpc.{AsRaw, AsReal}

trait AsyncEffect[F[_]] {
  def asAsync[A](f: F[A]): Async[A]
  def fromAsync[A](a: Async[A]): F[A]
}
object AsyncEffect {
  def apply[F[_]](implicit af: AsyncEffect[F]): AsyncEffect[F] = af

  implicit val futureAsyncEffect: AsyncEffect[Future] = new AsyncEffect[Future] {
    def asAsync[A](f: Future[A]): Async[A] =
      callback => f.onCompleteNow(callback)

    def fromAsync[A](a: Async[A]): Future[A] = {
      val prom = Promise[A]
      a(prom.complete)
      prom.future
    }
  }
}

object PolyRestImplicits extends GenCodecRestImplicits {
  implicit def asyncEffectAsResponse[F[_] : AsyncEffect, A](implicit asResp: AsRaw[RestResponse, A]): AsRaw[Async[RestResponse], Try[F[A]]] =
    AsRaw.create(tfa => Async.map(tfa.fold(Async.failed, fa => AsyncEffect[F].asAsync(fa)))(asResp.asRaw))

  implicit def asyncEffectFromResponse[F[_] : AsyncEffect, A](implicit asResp: AsReal[RestResponse, A]): AsReal[Async[RestResponse], Try[F[A]]] =
    AsReal.create(asyncResp => Success(AsyncEffect[F].fromAsync(Async.map(asyncResp)(asResp.asReal))))

  implicit def asyncEffectHttpResponseType[F[_] : AsyncEffect, A]: HttpResponseType[F[A]] =
    HttpResponseType()

  implicit def asyncEffectRestResultType[F[_] : AsyncEffect, A: RestResponses]: RestResultType[F[A]] =
    RestResultType(RestResponses[A].responses)
}

trait PolyRestApiInstances[T[_[_]]] {
  def asRawRest[F[_] : AsyncEffect]: RawRest.AsRawRpc[T[F]]
  def fromRawRest[F[_] : AsyncEffect]: RawRest.AsRealRpc[T[F]]
  def restMetadata[F[_] : AsyncEffect]: RestMetadata[T[F]]
  def openapiMetadata[F[_] : AsyncEffect]: OpenApiMetadata[T[F]]
}

abstract class PolyRestApiCompanion[T[_[_]]](implicit
  instances: MacroInstances[PolyRestImplicits.type, PolyRestApiInstances[T]]
) {
  private lazy val inst = instances(PolyRestImplicits, this)
  implicit def asRawRest[F[_] : AsyncEffect]: RawRest.AsRawRpc[T[F]] = inst.asRawRest
  implicit def fromRawRest[F[_] : AsyncEffect]: RawRest.AsRealRpc[T[F]] = inst.fromRawRest
  implicit def restMetadata[F[_] : AsyncEffect]: RestMetadata[T[F]] = inst.restMetadata
  implicit def openapiMetadata[F[_] : AsyncEffect]: OpenApiMetadata[T[F]] = inst.openapiMetadata
}

trait PolyRestApi[F[_]] {
  def postThis(thing: String): F[Int]
}
object PolyRestApi extends PolyRestApiCompanion[PolyRestApi]
