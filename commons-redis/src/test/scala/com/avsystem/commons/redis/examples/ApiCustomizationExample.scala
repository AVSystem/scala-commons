package com.avsystem.commons
package redis.examples

import akka.actor.ActorSystem
import akka.util.ByteString
import com.avsystem.commons.redis._
import com.avsystem.commons.serialization.GenCodec

import scala.concurrent.duration._

/**
  * Example which shows how to customize your Redis API in order to use types different from the default ones used
  * to represent keys, hash keys and values in Redis commands.
  */
object ApiCustomizationExample extends App {
  implicit val actorSystem: ActorSystem = ActorSystem()

  val client = new RedisNodeClient
  // By default, the driver provides textual and binary API variants, e.g.
  val textualApi = RedisApi.Node.Async.StringTyped(client)
  val binaryApi = RedisApi.Node.Async.BinaryTyped(client)
  // As you can see above, both API variants reuse the same client

  // Textual API uses String as key, hash key and value type
  def getKeyTextual: Future[Opt[String]] = textualApi.get("key")
  // Binary API uses ByteString as key, hash key and value type
  def getKeyBinary: Future[Opt[ByteString]] = binaryApi.get(ByteString("key"))

  // You can create your own API variants which use different types for keys, hash keys and values
  // These types must be serializable to binary form (and de-serializable from it), which is expressed by
  // `RedisDataCodec` typeclass.
  // By default, `RedisDataCodec` is provided for many simple types and all types which have a `GenCodec` instance.
  case class Person(
    name: String,
    birthYear: Int
  )
  object Person {
    implicit val codec: GenCodec[Person] = GenCodec.materialize[Person]
  }

  val personApi = new RedisApi.Node.Async[String, String, Person, Map[String, String]](client)

  def storePerson(person: Person): Future[Boolean] =
    personApi.set(person.name, person)

  // It is also possible to customize types "on the fly", without having to create completely separate API variant
  def storePerson2(person: Person): Future[Boolean] =
    textualApi.valueType[Person].set(person.name, person)
}
