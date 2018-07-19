# REST framework

The commons library contains an RPC based REST framework for defining REST services using Scala traits.
It may be used for implementing both client and server side and works in both JVM and JS, as long as
appropriate network layer is implemented. For JVM, Jetty-based implementations for client and server
are provided.

The core of REST framework is platform independent and network-implementation indepenedent and therefore
has no external dependencies. Because of that, it's a part of `commons-core` module. This is enough to
be able to define REST interfaces. But if you want to expose your REST interface through an actual HTTP
server or have an actual HTTP client for that interface, you need separate implementations for that.
The `commons-jetty` module provides Jetty-based implementations for JVM.

## Quickstart example

First, make sure appropriate dependencies are configured for your project (assuming SBT):

```scala
val commonsVersion: String = ??? // appropriate version of scala-commons here
libraryDependencies ++= Seq(
  "com.avsystem.commons" %% "commons-core" % commonsVersion,
  "com.avsystem.commons" %% "commons-jetty" % commonsVersion
)
```

Then, define some trivial REST interface:

```scala
import com.avsystem.commons.rest._

trait UserApi {
  /** Returns ID of newly created user */
  def createUser(name: String, birthYear: Int): Future[String]
}
object UserApi extends DefaultRestApiCompanion[UserApi]
```

Then, implement it on server side and expose it on localhost port 9090 using Jetty:

```scala
import com.avsystem.commons.jetty.rest.RestHandler
import org.eclipse.jetty.server.Server

class UserApiImpl extends UserApi {
  def createUser(name: String, birthYear: Int) = Future.successful(s"$name-ID")
}

object ServerMain {
  def main(args: Array[String]): Unit = {
    val server = new Server(9090)
    server.setHandler(RestHandler[UserApi](new UserApiImpl))
    server.start()
    server.join()
  }
}
```

Finally, obtain a client proxy for your API using Jetty HTTP client and make a call:

```scala
import com.avsystem.commons.jetty.rest.RestClient
import org.eclipse.jetty.client.HttpClient

import scala.concurrent.Await
import scala.concurrent.duration._

object ClientMain {
  def main(args: Array[String]): Unit = {
    val client = new HttpClient
    client.start()

    val proxy = RestClient[UserApi](client, "http://localhost:9090/")

    // just for this example, normally it's not recommended
    import scala.concurrent.ExecutionContext.Implicits.global

    val result = proxy.createUser("Fred", 1990)
      .andThen { case _ => client.stop() }
      .andThen {
        case Success(id) => println(s"User $id created")
        case Failure(cause) => cause.printStackTrace()
      }

    // just wait until future is complete so that main thread doesn't finish prematurely
    Await.result(result, 10.seconds)
  }
}
```

If we look at HTTP traffic, that's what we'll see:

Request:
```
POST http://localhost:9090/createUser HTTP/1.1
Accept-Encoding: gzip
User-Agent: Jetty/9.3.23.v20180228
Host: localhost:9090
Content-Type: application/json;charset=utf-8
Content-Length: 32

{"name":"Fred","birthYear":1990}
```

Response:
```
HTTP/1.1 200 OK
Date: Wed, 18 Jul 2018 11:43:08 GMT
Content-Type: application/json;charset=utf-8
Content-Length: 9
Server: Jetty(9.3.23.v20180228)

"Fred-ID"
```

## REST API traits

As we saw in the quickstart example, REST API is defined by a Scala trait adjusted with annotations.
This approach is analogous to various well-established REST frameworks for other languages, e.g. JAX-RS for Java.
However, such frameworks are usually based on runtime reflection while in Scala it can be
done using compile-time reflection through macros which offers several advantages:

* platform independency - REST traits are understood by both ScalaJVM and ScalaJS
* full type information - compile-time reflection is not limited by type erasure
* type safety - compile-time reflection can perform thorough validation of REST traits and
  raise compilation errors in case anything is wrong
* pluggable typeclass based serialization - for serialization of REST parameters and results,
  typeclasses are used which also offers strong compile-time safety. If any of your parameters or
  method results cannot be serialized, a detailed compilation error will be raised.

### Companion objects

In order for a trait to be understood as REST API, it must have a well defined companion object that contains
appropriate implicits:

* in order to expose REST API on a server, implicit instances of `RawRest.AsRawRpc` and `RestMetadata` for API trait are required.
* in order to use REST API client, implicit instances of `RawRest.AsRealRpc` and `RestMetadata` for API trait are required.
* when API trait is used by both client and server, `RawRest.AsRawRpc` and `RawRest.AsRealRpc` may be provided by a single
  combined instance of `RawRest.AsRawRealRpc` for API trait.

Usually there is no need to declare these implicit instances manually because you can use one of the convenience
base classes for REST API companion objects, e.g.

```scala
import com.avsystem.commons.rest._

trait MyApi { ... }
object MyApi extends DefaultRestApiCompanion[MyApi]
```

`DefaultRestApiCompanion` takes a magic implicit parameter generated by a macro which will effectively
materialize all the necessary typeclass instances mentioned earlier. The "`Default`" in its name means that
`DefaultRestImplicits` is used as a provider of serialization-related implicits. See [Serialization](#serialization)
for more details on customizing serialization.

`DefaultRestApiCompanion` provides all the implicit instances necessary for both the client and server.
If you intend to use your API trait only on the server or only on the client, you may want to use more lightweight
`DefaultRestClientApiCompanion` or `DefaultRestServerApiCompanion`. This may help you reduce the amount of macro
generated code and make compilation faster.

#### Manual declaration of implicits

On less frequent occasions you might be unable to use one of the companion base classes. In such situations you must
declare all the implicit instances manually (however, they will still be implemented with a macro).
For example:

```scala
import com.avsystem.commons.rest._

trait MyApi { ... }
object GenericApi {
  import DefaultRestImplicits._
  implicit val restAsRawReal: RawRest.AsRawRealRpc[MyApi] = RawRest.materializeAsRawReal
  implicit val restMetadata: RestMetadata[MyApi] = RestMetadata.materializeForRpc
}
```

This is usually necessary when implicit instances need some additional implicit dependencies or when
the API trait is generic (has type parameters).

### HTTP REST methods

REST macro engine inspects API trait and looks for all abstract methods. It then tries to translate every abstract
method into a HTTP REST call.

* By default (if not annotated explicitly) each method is interpreted as HTTP `POST`.
* Method name is appended to the URL path.
* Every parameter is interpreted as part of the body - all the body parameters will be
  combined into a JSON object sent through HTTP body. However, for `GET` methods, every parameter
  is interpreted as URL query parameter while the body is empty.
* Result type of each method is typically expected to be a `Future` wrapping some
  arbitrary response type. This response type will be serialized into HTTP response which
  by default translates it into JSON and creates a `200 OK` response with `application/json`
  content type. If response type is `Unit` (method result type is `Future[Unit]`) then empty
  body is created when serializing and body is ignored when deseriarlizing.

For details on how exactly serialization works and how to customize it, see [Serialization](#serialization).
Note that if you don't want to use `Future`, this customization also allows you to use other wrappers for method result types.

### Choosing HTTP method

As mentioned earlier, each trait method is by default translated into a `POST` request.
You can specify which HTTP method you want by explicitly annotating trait method as
`@GET`/`@POST`/`@PATCH`/`@PUT` or `@DELETE` (from `com.avsystem.commons.rest` package).

```scala
@DELETE def deleteUser(id: String): Future[Unit]
```

#### `GET` methods

Trait method annotated as `@GET` is interpreted somewhat differently from other HTTP methods.
Its parameters are interpreted as _query_ parameters rather than _body_ parameters. For example:

```scala
@GET def getUsername(id: String): Future[String]
```

Calling `getUsername("ID")` on the client will result in HTTP request:

```
GET http://localhost:9090/getUsername?userId=ID HTTP/1.1
Accept-Encoding: gzip
User-Agent: Jetty/9.3.23.v20180228
Host: localhost:9090

```

### Customizing paths

By default, method name is appended to URL path when translating method call to HTTP request.
This can be customized. Every annotation specifying HTTP method (e.g. `GET`) takes an optional
`path` argument that you can use to customize your path:

```scala
@GET("username") def getUsername(id: String): Future[String]
```

The specified path may be multipart (it may contain slashes) or it may even be empty.
However, for server-side APIs all paths must be unambiguous, i.e. there must not be more
than one method translating to the same path. This is validated in runtime, upon
creating a server.

Empty paths may be especially useful for [prefix methods](#prefix-methods).

### Path parameters

If a parameter of REST API trait method is annotated as `@Path`, its value is
appended to URL path rather than translated into query parameter or body part.

```scala
@GET("username") def getUsername(@Path id: String): Future[String]
```

Calling `getUsername("ID")` will make a HTTP request on path `username/ID`.

If there are multiple `@Path` parameters, their values are appended to the path in the
order of declaration. Each path parameters may also optionally specify a _path suffix_ that
will be appended to path after value of each parameter:

```scala
@GET("users") def getUsername(@Path(pathSuffix = "name") id: String): Future[String]
```

Calling `getUsername("ID")` will make a HTTP request on path `users/ID/name`.

This way you can model completely arbitrary path patterns.

Values of path parameters are serialized into `PathValue` objects,
see [Serialization](#serializaton) for more details.

### Query parameters

You may explicitly request that some parameter is translated into URL query parameter
using `@Query` annotation. As mentioned earlier, parameters of `GET` methods are treated
as query parameters by default, so this is only strictly necessary for non-`GET` methods.

`@Query` annotation also takes optional `name` parameter which may be specified to customize
URL parameter name. If not specified, trait method parameter name is used.

Values of query parameters are serialized into `QueryValue` objects,
see [Serialization](#serializaton) for more details.

### Header parameters

You may also request that some parameter is translated into a HTTP header using `@Header` annotation.
It takes an obligatory `name` argument that specifies HTTP header name.

Values of header parameters are serialized into `HeaderValue` objects,
see [Serialization](#serializaton) for more details.

### JSON Body parameters

As mentioned earlier, every parameter of non-`GET` API trait method is interpreted as a field
of a JSON object sent as HTTP body. Just like for path, query and header parameters, there is a
`@JsonBodyParam` annotation which requests this explicitly. However, it only makes sense to use this
annotation when one wants to customize the field name because `GET` methods do not accept body parameters.
A method annotated as `@GET` having a parameter annotated as `@JsonBodyParam` will be rejected by REST
macro engine.

JSON body parameters are serialized into `JsonValue` objects,
see [Serialization](#serializaton) for more details.

### Single body parameters

Every non-`GET` API method may also take a single parameter annotated as `@Body` in place of multiple
unannotated parameters (implicitly interpreted as `@JsonBodyParam`s). This way the value of that parameter
will be serialized straight into HTTP body. This gives you full control over the contents sent in HTTP body
and their format (i.e. it no longer needs to be `application/json`).

Single body parameters are serialized into `HttpBody` objects,
see [Serialization](#serializaton) for more details.

### Prefix methods

If a method in REST API trait doesn't return `Future[T]` (or other type that
properly translates to asynchronous HTTP response) then it may also be interpreted as _prefix_ method.

Prefix methods are methods that return other REST API traits. They are useful for:

* capturing common path or path/query/header parameters in a single prefix call
* splitting your REST API into multiple traits in order to better organize it

Just like HTTP API methods (`GET`, `POST`, etc.), prefix methods have their own
annotation that can be used explicitly when you want your trait method to be treated as
a prefix method. This annotation is `@Prefix` and just like HTTP method annotations, it
takes an optional `path` parameter. If you don't need to specify path explicitly then
annotation is not necessary as long as your method returns a valid REST API trait
(where "valid" is determined by presence of appropriate implicits -
see [companion objects](#companion-objects)).

Prefix methods may take parameters. They are interpreted as path parameters by default,
but they may also be annotated as `@Query` or `@Header`. Prefix methods must not take
body parameters.

Path and parameters collected by a prefix method will be prepended/added
to the HTTP request generated by a HTTP method call on the API trait returned by this
prefix method. This way prefix methods "contribute" to the final HTTP requests.

However, sometimes it may also be useful to create completely "transparent" prefix methods -
prefix methods with empty path and no parameters. This is useful when you want to refactor your
REST API trait by grouping methods into multiple, separate traits without changing the
format of HTTP requests.

Example of prefix method that adds authentication header to the overall API:

```scala
trait UserApi { ... }
object UserApi extends DefaultRestApiCompanion[UserApi]

trait RootApi {
  @Prefix("") def auth(@Header("Authorization") token: String): UserApi
}
object RootApi extends DefaultRestApiCompanion[RootApi]
```

## Serialization

REST macro engine must be able to generate code that serializes and deserializes
every parameter value and every method result into appropriate raw values which can
be easily sent through network. Serialization in REST framework is typeclass based,
which is a typical, functional and typesafe approach to serialization in Scala.

Examples of typeclass based serialization libraries include [GenCodec](GenCodec.md)
(which is the default serialization used by this REST framework), [circe](https://circe.github.io/circe/)
(one of the most popular JSON libraries for Scala) or [µPickle](http://www.lihaoyi.com/upickle/).
Any of these solutions can be plugged into REST framework.

### Real and raw values

Depending on the context where a type is used in a REST API trait, it will be serialized to a different
_raw value_:

* path/query/header parameters are serialized as `PathValue`/`QueryValue`/`HeaderValue`
* JSON body parameters are serialized as `JsonValue`
* Single body parameters are serialized as `HttpBody`
* Response types are serialized as `RestResponse`
* Prefix result types (other REST API traits) are "serialized" as `RawRest`

When a macro needs to serialize a value of some type (let's call it `Real`) to one of these raw types
listed above (let's call it `Raw`) then it looks for an implicit instance of `AsRaw[Raw, Real]`.
In the same manner, an implicit instance of `AsReal[Raw, Real]` is used for deserialization.
Additionally, if there is an implicit instance of `AsRawReal[Raw, Real]` then it serves both purposes.

These implicit instances may come from multiple sources:

* implicit scope of the `Raw` type (e.g. its companion object)
* implicit scope of the `Real` type (e.g. its companion object)
* implicits plugged by REST API trait companion
  (e.g. `DefaultRestApiCompanion` plugs in `DefaultRestImplicits`)
* imports

Of course, these implicits may also depend on other implicits which effectively means that
you can use whatever typeclass-based serialization library you want.
For example, you can define an instance of `AsRaw[JsonValue, Real]` which actually uses
`Encoder[Real]` from [circe](https://circe.github.io/circe/). See [Customizing serialization](#customizing-serialization)
for more details.

### Path, query and header serialization

Path, query and header parameter values are serialized into `PathValue`/`QueryValue`/`HeaderValue`.
These three classes are all simple `String` wrappers. Thanks to the fact that they are distinct types,
you can have completely different serialization defined for each one of them if you need.

There are no "global" implicits defined for these raw types. They must be either imported, defined by each
"real" type or plugged in by REST API trait companion. For example, the `DefaultRestApiCompanion` and its
variations automatically provide serialization to `PathValue`/`QueryValue`/`HeaderValue` based on `GenKeyCodec`
and additional instances for `Float` and `Double`. This effectively provides serialization for all the
primitive types, its Java boxed counterparts, `String`, all `NamedEnum`s, Java enums and `Timestamp`.
It's also easy to provide path/query/header serialization for any type which has a natural, unambiguous textual
representation.

Serialized values of path & query parameters are automatically URL-encoded when being embedded into
HTTP requests. This means that serialization should not worry about that.

### JSON body parameter serialization

JSON body parameters are serialized into `JsonValue` which is also a simple wrapper class over `String`,
but is importantly distinct from `PathValue`/`QueryValue`/`HeaderValue` because it must always contain
a valid JSON string. This is required because JSON body parameters are ultimately composed into a single
JSON object sent as HTTP body.

There are no "global" implicits defined for `JsonValue` - JSON serialization must be either imported,
defined by each "real" type manually or plugged in by REST API trait companion. For example,
`DefaultRestApiCompanion` and its variations automatically provide serialization to `JsonValue` based
on `GenCodec`, which means that if `DefaultRestApiCompanion` is used then every type used in REST API trait
that has a `GenCodec` instance will be serializable to `JsonValue`.

### Single body serialization

Single body parameters (annotated as `@Body`) are serialized straight into `HttpBody`, which encapsulates
not only raw content but also MIME type. This way you can define custom body serializations for your types and
you are not limited to `application/json`.

By default (if there are no more specific implicit instances defined),
serialization to `HttpBody` falls back to `JsonValue` and simply wraps JSON string into HTTP body
with `application/json` type. This means that all types serializable to `JsonValue` are automatically
serializable to `HttpBody`.

### Result serialization

Result type of every REST API method is "serialized" into `Future[RestResponse]`, which means that
macro engine looks for an implicit instance of `AsRaw/AsReal[Future[RestResponse], MethodResultType]`.
`RestResponse` is a simple type that encapsulates HTTP status code and body.

However, `RestResponse` companion object defines a default implicit instance of `AsRaw/Real[Future[RestResponse], Future[T]]`
which depends on implicit `AsRaw/Real[HttpBody, T]`. This default serialization always uses 200 as a status code
for successful `Future[T]` values. In practice this means that if your REST API method returns
`Future[T]` then your `T` type must be serializable to `HttpBody`. Additionally, remember that all types serializable to
`JsonValue` are automatically serializable to `HttpBody`.

You might want to define custom serialization straight into `Future[RestResponse]` when:

* You don't want to use `Future` in your REST API traits but some other type that encapsulates asynchronous
  computation, e.g. [Monix Task](https://monix.io/docs/2x/eval/task.html)
* Result of your HTTP method determines HTTP status code that you want to be sent back to the client.

### Customizing serialization

#### Introduction

When Scala compiler needs to find an implicit, it searches two scopes: _lexical_ scope and _implicit_ scope.

_Lexical scope_ is made of locally visible and imported implicits. It has priority over implicit scope -
implicit scope is searched only when implicit could not be found in lexical scope.

_Implicit scope_ is made of companion objects of all traits and classes _associated_ with the
type of implicit being searched for. Consult [Scala Language Specification](https://www.scala-lang.org/files/archive/spec/2.12/07-implicits.html)
for precise definition of the word "_associated_". As an example, implicit scope of type `AsRaw[JsonValue,MyClass]` is
made of companion objects of `AsRaw`, `JsonValue`, `MyClass` + companion objects of all supertraits, superclasses and
enclosing traits/classes of `MyClass`.

Implicits defined in _implicit scope_ are effectively global and don't need to be imported.

#### Plugging in entirely custom serialization

REST framework deliberately provides **no** default implicits for serialization and deserialization.
Instead, it introduces a mechanism through which serialization implicits are injected by
[companion objects](#companion-objects) of REST API traits. Thanks to this mechanism REST framework is
not bound to any specific serialization library. At the same time it provides a concise method to inject
serialization implicits that does not require importing them explicitly.

An example usage of this mechanism is `DefaultRestApiCompanion` which injects [`GenCodec`](GenCodec.md)-based
serialization.

Let's say you want to use e.g. [circe](https://circe.github.io/circe/) for serialization to `JsonValue`.

First, define a trait that contains implicits which translate Circe's `Encoder` and `Decoder` into
appropriate instances of `AsReal`/`AsRaw`:

```scala
import com.avsystem.commons.rest._
import com.avsystem.commons.rpc._
import io.circe._
import io.circe.parser._
import io.circe.syntax._

trait CirceRestImplicits {
  implicit def encoderBasedAsRawJson[T: Encoder]: Fallback[AsRaw[JsonValue, T]] =
    Fallback(AsRaw.create(v => JsonValue(v.asJson.noSpaces)))
  implicit def decoderBasedJsonAsReal[T: Decoder]: Fallback[AsReal[JsonValue, T]] =
    Fallback(AsReal.create(json => decode(json.value).fold(throw _, identity)))
}
object CirceRestImplicits extends CirceRestImplicits
```

Note that implicits are wrapped into `Fallback`. This is not strictly required, but it's recommended
because these implicits ultimately will have to be imported into lexical scope, even if a macro does it
for us. However, we don't want these implicits to have higher priority than implicits from companion objects
of some concrete classes which need custom serialization. Because of that, we wrap our implicits into
`Fallback` which keeps them visible but without elevated priority.

Now, in order to define a REST API trait that uses Circe-based serialization, you must appropriately
inject it into its companion object:

```scala
trait MyRestApi { ... }
object MyRestApi extends RestApiCompanion[CirceRestImplicits, MyRestApi](CirceRestImplicits)
```

If you happen to use this often (e.g. because you always want to use Circe) then it may be useful
to create convenience companion base class just for Circe:

```scala
abstract class CirceRestApiCompanion[Real](
  implicit inst: RpcMacroInstances[CirceRestImplicits, FullInstances, Real])
) extends RestApiCompanion[CirceRestImplicits, Real](CirceRestImplicits)
```

Now you can define your trait more concisely as:

```scala
trait MyRestApi { ... }
object MyRestApi extends CirceRestApiCompanion[MyRestApi]
```

#### Customizing serialization for your own type

If you need to write manual serialization for your own type, the easiest way to do this is to
provide appropriate implicit in its companion object:

```scala
class MyClass { ... }
object MyClass {
  implicit val jsonAsRawReal: AsRawReal[JsonValue, MyClass] = AsRawReal.create(...)
}
```

#### Providing serialization for third party type

If you need to define serialization implicits for a third party type, you can't do it through
implicit scope because you can't modify its companion object. Instead, you can adjust implicits injected
into REST API trait companion object.

Assume that companion objects of your REST API traits normally extend `DefaultRestApiCompanion`, i.e.
`GenCodec`-based serialization is used. Now, you can extend `DefaultRestImplicits` to add serialization for
third party types:

```scala
trait EnhancedRestImplicits extends DefaultRestImplicits {
  implicit val thirdPartyJsonAsRawReal: AsRawReal[JsonValue, ThirdParty] =
    AsRawReal.create(...)
}
object EnhancedRestImplicits extends EnhancedRestImplicits
```

Then, you need to define your REST API trait as:

```scala
trait MyRestApi { ... }
object MyRestApi extends RestApiCompanion[EnhancedRestImplicits, MyRestApi](EnhancedRestImplicits)
```

## API evolution

## Implementing backends
