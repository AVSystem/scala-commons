# Typesafe RPC & proxy framework

`commons-shared` module contains a set of tools for implementing type-safe remote interfaces. It is not bound to any particular transport protocol or serialization method. Its purpose is to provide type-safety layer over "raw" transport layer. This is primarily provided by automatic translation between "real" RPC interfaces (represented by regular, typesafe Scala traits) and "raw" RPC interfaces where an invocation is already represented in a format suitable for sending over a network or other low-level transport mechanism that accepts raw bytes or strings.

RPC framework is cross-compiled for JVM and JS, which makes it especially useful for implementing communication layer between client and server in ScalaJS applications.

## Table of Contents

  * [RPC traits](#rpc-traits)
      * [Overloaded remote methods](#overloaded-remote-methods)
      * [Non-abstract members](#non-abstract-members)
  * [Choosing a serialization mechanism](#choosing-a-serialization-mechanism)
  * [RPC client implementation](#rpc-client-implementation)
    * [Implementing the transport](#implementing-the-transport)
    * [Wrapping raw RPC into a "real" proxy](#wrapping-raw-rpc-into-a-real-proxy)
      * [Type safety](#type-safety)
      * [Internals](#internals)
  * [RPC server implementation](#rpc-server-implementation)
    * [RPC implementation](#rpc-implementation)
    * [Wrapping "real" RCP implementation into a raw RPC](#wrapping-real-rcp-implementation-into-a-raw-rpc)
      * [Type safety](#type-safety-1)
      * [Internals](#internals-1)
  * [RPC metadata](#rpc-metadata)
    * [Example](#example)
  * [Declaring typeclass instances explicitly](#declaring-typeclass-instances-explicitly)

## RPC traits

Suppose you have a client-server application where client wants to invoke remote operations on the server. The notions of "client" and "server" are abstract here, since the framework does not assume any particular transport method. This set of operations must be represented using a trait (or abstract class) annotated as `@RPC`:

```scala
import com.avsystem.commons.rpc.RPC

@RPC trait UserService {
}
```

The RPC framework interprets all **abstract** methods (`def`s) declared in this interface as remote methods. There are three types of remote methods allowed:
* procedures: methods with `Unit` return type are interpreted as "fire and forget" operations, i.e. the server doesn't return any result or success acknowledgement
* functions: methods with `Future[T]` return type are interpreted as functions that return some result. Result must always be wrapped in a `Future`, because the communication layer between client and server may be asynchronous. The server-side implementation of a function may also be asynchronous.
* getters: methods that return another RPC interface (trait or abstract class annotated as `@RPC`). This way one RPC interface may aggregate many other sub-interfaces.

Every remote method may take parameters, including implicit ones. However, every parameter type must be "serializable" so that parameter values can be sent over network in raw form. What "serializable" means depends on the serialization library chosen. We will elaborate on that in the next section.

Remote methods can **not** have type parameters (generics).

Example of an RPC trait:

```scala
import com.avsystem.commons.rpc.RPC

case class User(id: String, login: String, birthYear: Int)
@RPC trait UserService {
  def addIfAbsent(user: User): Unit
  def findById(id: String): Future[User]
}
@RPC trait Services {
  def userService: UserService
}
```

#### Overloaded remote methods

It is also possible to overload RPC methods. However, in order to do it, one must annotate all the overloads with `@RPCName` annotation to give each overloaded variant unique name.

#### Non-abstract members

RPC interface traits may also contain non-abstract methods and fields. These will be simply ignored by the RPC framework, which means they will be invoked locally. This may be either on client or server:
* When local method is called by the client (on the proxy implementation), the method is invoked on the client. However, that method may by itself call other, possibly remote methods, so you can't assume that a non-abstract method doesn't involve comunication with the server.
* When local method is called by the server (on the actual implementation), the method is invoked on the server. In particular, a server-side implementation of a remote method may use a local method.

RPC traits are a part of code shared between the client and the server. For example, in a ScalaJS application that contacts a Scala(JVM) server, RPC traits would most likely be [cross-compiled](https://www.scala-js.org/doc/project/cross-build.html). Server-side code is supposed to implement these interfaces while client-side code is supposed to use them through auto-generated proxies (described later in this documentation).

## Choosing a serialization mechanism

RPC framework requires that every parameter of every RPC method and result type of every RPC function is serializable. What it means exactly is determined by serialization mechanism used. The framework assumes that serialization is typeclass-based.
An example of such mechanism is the [`GenCodec` framework](GenCodec.md) which is also a part of AVSystem commons. Another, external example of such serialization library is [µPickle](http://www.lihaoyi.com/upickle-pprint/upickle/) for JSON serialization.

In typeclass-based serialization there usually is some typeclass, e.g. `Writer` for serializing and e.g. `Reader` for deserializing (`Writer` and `Reader` may be the same typeclass, e.g. `Codec`). Type `T` is serializable when there is an instance of typeclass `Writer` for type `T`, i.e. there is an implicit value of type `Writer[T]` available. Similarly, `T` is de-serializable when there's an implicit instance of `Reader` for it.

The choice of actual `Reader` and `Writer` typeclasses is yours. For example, when using [`GenCodec` framework](GenCodec.md), the typeclass for both writing and reading could be `GenCodec.Auto` (for fully automatically derived codecs).

You will also need to choose a "raw value" type, i.e. the format to which parameter values and function results are serialized. This is usually determined by the serialization method used. For example, when using *µPickle*, the raw value type would be either `Js.Value` (Scala JSON representation) or `String` (JSON string). When using the `GenCodec` framework with `SimpleValueWriter`/`SimpleValueReader`, the raw type would simply be `Any`.

After choosing the types, you'll finally have to implement the serialization and deserialization methods that actually convert between normal values and raw values (using `Reader`/`Writer` typeclass instances). Usually, this is just a simple call that delegates the job to the typeclass.

Serialization mechanism is chosen by creating an object implementing `StandardRPCFramework` trait. Here's an example for `GenCodec`:

```scala
import com.avsystem.commons.serialization._
import com.avsystem.commons.rpc._

object SimpleRPCFramework extends StandardRPCFramework {
  type RawValue = Any
  type Reader[T] = GenCodec.Auto[T]
  type Writer[T] = GenCodec.Auto[T]
  
  def write[T: Writer](value: T): RawValue = {
    var result: Any = null
    GenCodec.autoWrite[T](new SimpleValueOutput(result = _), value)
    result
  }
  
  def read[T: Reader](raw: RawValue): T = 
    GenCodec.autoRead[T](new SimpleValueInput(raw))
}
```

It is specifically recommended to implement `StandardRPCFramework` as an `object`. This is because it has types inside it that need to be referred to. The object is going to serve as a namespace or "package". From on now, everything in the following examples will happen in the "scope" of `SimpleRPCFramework`.

**NOTE**: The base trait is called `StandardRPCFramework` because it's a specialized version of more generic `RPCFramework`. The "standardness" of `StandardRPCFramework` is its support for procedures, functions and getters (RPC methods returning `Unit`, `Future` or another RPC interface). When directly extending `RPCFramework`, it is possible to create frameworks that support other types of RPC methods. However, this is not yet covered by this documentation.

## RPC client implementation

When client wants to call an RPC method, it needs an RPC proxy implementation, i.e. an implementation of RPC interface that will translate the calls into raw form (which includes serializing the parameters) that can be sent to the server (e.g. in an AJAX call).

### Implementing the transport

The real-to-raw translation is done by the RPC framework automatically. The programmer's responsibility is to implement the actual sending of requests and receiving of responses. This is done by implementing a `RawRPC` (which is bound to the framework object, so it's actually `SimpleRPCFramework.RawRPC` in our examples). `RawRPC` has three methods to implement: `fire` to handle RPC prodedures (fire-and-forget), `call` for functions and `get` for getters.

```scala
trait RawRPC {
  def fire(rpcName: String, argLists: List[List[RawValue]]): Unit
  def call(rpcName: String, argLists: List[List[RawValue]]): Future[RawValue]
  def get(rpcName: String, argLists: List[List[RawValue]]): RawRPC
}
```

As you can see, each invocation is already represented in raw form. RPC method used is identified with a `String` and all the parameters are already serialized into `RawValue`. This is something easy to send through network. Function results are also expected to be returned as `RawValue`s - the RPC framework automatically handles their deserialization.

Implementation of `fire` and `call` should contain the actual network operations. We omit them in this example as it's not in the scope of this guide. We can only promise that implementing them as AJAX calls using ScalaJS is fairly straightforward.

What's left is the `get` method. It's supposed to return a raw form of RPC subinterface. Usually, `get` doesn't require any network operations but simply aggregates RPC name and arguments so that they can be sent along with the actual `fire` or `call` invocation that's eventually done on some subinterface. Here's an example:

```scala
import SimpleRPCFramework._

// `RawInvocation` comes from `SimpleRPCFramework` and contains rpcName and argLists of all invoked getters
class AjaxRawRPC(getterChain: List[RawInvocation]) extends RawRPC {
  def fire(rpcName: String, argLists: List[List[RawValue]]): Unit = { 
    // send RPC invocation along with the getter chain using AJAX
  } 
  def call(rpcName: String, argLists: List[List[RawValue]]): Future[RawValue] = {
    // send RPC invocation along with the getter chain using AJAX
  }
  // `get` method simply adds the getter invocation to the stack and returns the same implementation of `RawRPC`
  def get(rpcName: String, argLists: List[List[RawValue]]): RawRPC =
    new AjaxRawRPC(RawInvocation(rpcName, argLists) :: getterChain)
}
```

### Wrapping raw RPC into a "real" proxy

After you have a `RawRPC` implemented, you can wrap it into the actual implementation of your original RPC interface. This implementation will be a proxy that will delegate actual, nice, statically-typed operations from your original interface into raw calls on your `RawRPC`.

To generate such proxy, use the `AsRealRPC` typeclass bound to your `StandardRPCFramework`:

```scala
import SimpleRPCFramework._

val rawRpc = new AjaxRawRPC(Nil)
val realRpc: Services = AsRealRPC[Services].asReal(rawRpc)
```

`realRpc` is now a fully usable implementation of `Services` trait. Invocations of its methods will be translated into raw invocations on `AjaxRawRPC`. For example, the chained call:

```scala
realRpc.userService.addIfAbsent(User("123", "ghik", 1990))
```

will actually result in:

```scala
rawRpc.get("userService", Nil).fire("addIfAbsent", List(List(Map("id" -> "123", "login" -> "ghik", "birthYear" -> 1990)))
```

being executed. 

The `Map` passed as a raw argument to `fire` method is the serialized format of `User` case class. Of course, this format may be completely different depending on the serialization mechanism (remember that examples use `SimpleRPCFramework` which uses `GenCodec`s and `SimpleValueInput`/`SimpleValueOutput`).

#### Type safety

Instances of `AsRealRPC` typeclass are provided automatically by a macro which uses compile-time reflection to inspect the trait and generate the proxy code. During that, it also thoroughly validates the interface against the rules described in the section about RPC traits. If the programmer makes any mistake, he/she will be immediately notified with a compilation error. The following situations will cause the compilation to fail:
* some abstract method has wrong signature (takes type parameters or has wrong return type)
* any of the RPC parameters is not serializable (no instance of `Writer` typeclass is found for its type)
* any of the function results is not de-serializable (no instance of `Reader` typeclass is found for its type)
* some of the RPC methods have conflicting names (e.g. overloaded method not distinguished with `@RPCName` annotation)

#### Internals

For better understanding of what's going on in the proxy implementation that translates real calls into raw ones, here's the simplified code of macro-generated `AsRealRPC[Services]` instance:

```scala
import SimpleRPCFramework._

val realRpc: Services = new AsRealRPC[Services] {
  def asReal(rawServicesRpc: RawRPC): Services = new Services {
    def userService: UserService = new AsRealRPC[UserService] {
      def asReal(rawUserServiceRpc: RawRPC): UserService = new UserService {
        def addIfAbsent(user: User): Unit = 
          rawUserServiceRpc.fire("addIfAbsent", List(List(write[User](user))))
        def findById(id: String): Future[User] = 
          rawUserServiceRpc.call("findById", List(List(write[String](id)))).map[User](raw => read[User](raw))
      }
    }.asReal(rawServicesRpc.get("userService", Nil))
  }
}.asReal(rawRpc)
```

## RPC server implementation

Now that we have a fully-functional RPC client, we also need the server-side to receive raw invocations sent by client-side `RawRPC` implementation, translate them into operations on actual RPC implementation and send back the result (for functions).

### RPC implementation

First, let's provide implementation of remote methods. This should be pretty straightforward, since it doesn't differ in any way from regular trait implementation. The only detail is that even though some function implementation doesn't need to be asynchronous, it still needs to wrap the result into a `Future`.

```scala
val realRpc = new Services {
  def userService = new UserService {
    def addIfAbsent(user: User): Unit = { /* perform DB operations */ }
    def findById(id: String): Future[User] = { /* perform DB operations */ }
  }
}
```

### Wrapping "real" RCP implementation into a raw RPC

On client side we had a `RawRPC` implemented by us that did the network operations. That raw RPC was auto-wrapped into "real" RPC interface using `AsRealRPC`. 

On the server side, we need to do exactly the opposite. We already have the real interface implemented, but we're going to be receiving raw RPC invocations from network and we need to somehow translate them into calls on the real interface.

This guide is, again, detached from any particular transport method, so let's just assume we have some trait, `RequestHandler` which we need to implement and register into the underlying network layer. The handler will be notified every time a RPC invocation is received from the client.

```scala
import SimpleRPCFramework._
trait RequestHandler {
  def handleFire(getterChain: List[RawInvocation], finalFire: RawInvocation): Unit
  def handleCall(getterChain: List[RawInvocation], finalCall: RawInvocation): Future[RawValue]
}
```

We need to implement that handler and pass the operations to `realRpc` implemented before. In order to do it, we can wrap `realRpc` into `RawRPC`. This is done by the `AsRawRPC` typeclass, complementary to previously used `AsRealRPC` on the client side. `RawRPC` accepts invocations in almost the same format as we receive them from network, so handler implementation should be straightforward.

```scala
import SimpleRPCFramework._
val realRpc: Services = ???
val rawRpc: RawRPC = AsRawRPC[Services].asRaw(realRpc)

object SimpleHandler extends RequestHandler {
  def handleFire(getterChain: List[RawInvocation], finalFire: RawInvocation): Unit =
    rawRpc.resolveGetterChain(getterChain).fire(finalFire.rpcName, finalFire.argLists)
  def handleCall(getterChain: List[RawInvocation], finalCall: RawInvocation): Future[RawValue] =
    rawRpc.resolveGetterChain(getterChain).call(finalFire.rpcName, finalFire.argLists)
}
```

#### Type safety

Just like for `AsRealRPC`, instances of `AsRawRPC` are macro-generated and perform thorough validation of RPC interfaces that should fail in compile-time if anything is invalid. Compilation error will be issued when:
* some abstract method has wrong signature (takes type parameters or has wrong return type)
* any of the RPC parameters is not de-serializable (no instance of `Reader` typeclass is found for its type)
* any of the function results is not serializable (no instance of `Writer` typeclass is found for its type)
* some of the RPC methods have conflicting names (e.g. overloaded method not distinguished with `@RPCName` annotation)

#### Internals

For better understanding of what's going on in the raw-to-real translation, here's the simplified code of macro-generated `AsRawRPC[Services]` instance:

```scala
import SimpleRPCFramework._

val rawRpc: RawRPC = new AsRawRPC[Services] {
  def asRaw(services: Services) = new RawRPC {
    def fire(rpcName: String, argLists: List[List[RawValue]]) = 
      fail("Services", "procedure", rpcName, argLists)
    def call(rpcName: String, argLists: List[List[RawValue]]) = 
      fail("Services", "function", rpcName, argLists)
    def get(rpcName: String, argLists: List[List[RawValue]]) = (rpcName, argLists) match {
      case ("userService", Nil) => new AsRawRPC[UserService] {
        def asRaw(userService: UserService) = new RawRPC {
          def fire(rpcName: String, argLists: List[List[RawValue]]) = (rpcName, argLists) match {
            case ("addIfAbsent", List(List(user))) => userService.addIfAbsent(read[User](user))
            case _ => fail("UserService", "procedure", rpcName, argLists)
          }
          def call(rpcName: String, argLists: List[List[RawValue]]) = (rpcName, argLists) match {
            case ("findById", List(List(id))) =>
              userService.findById(read[String](id)).map(user => write[User](user))
            case _ => fail("UserService", "function", rpcName, argLists)
          }
          def get(rpcName: String, argLists: List[List[RawValue]]) = 
            fail("UserService", "getter", rpcName, argLists)
        }
      }.asRaw(services.userService)
      case _ => fail("Services", "getter", rpcName, argLists)
    }
  }
}.asRaw(realRpc)
```

## RPC metadata

When implementing network layer for transporting RPC invocations, it is often useful to have some more information about the RPC interface and its operations. For example, `RawRPC` implementation may be interested in names of the parameters or some annotations applied on them. Information like that is provided by `RPCMetadata` typeclass. Having an instance `RPCMetadata[T]` for RPC trait `T`, you can extract following metadata from it:
* name of the RPC trait (simple trait or class name)
* annotations applied on the RPC trait, assuming they are a subclass of `MetadataAnnotation`
* signature metadata for every RPC method, which includes:
 * RPC name - method name by default unless overridden with `@RPCName` annotation
 * annotations applied on RPC method, assuming they are a subclass of `MetadataAnnotation`
 * metadata for every parameter, which includes:
   * parameter name
    * annotations applied on RPC method parameter, assuming they are a subclass of `MetadataAnnotation`
* for getters, `RPCMetadata` instance for RPC subinterface returned by the getter
 
As you can see, `RPCMetadata` provides similar information to standard Java reflection. However, `RPCMetadata` has various advantages over it, the most important one being **platform independency** - metadata is available in both JVM and JS, in the same format.

Instances of `RPCMetadata` are automatically generated with macros, using compile-time reflection.

### Example

Suppose some RPC methods need permission checking. We can express that an RPC method requires some permissions with an annotation:

```scala
import com.avsystem.commons.rpc._

case class RequiresPermissions(perms: String*) extends MetadataAnnotation

case class User(id: String, login: String, birthYear: Int)
@RPC trait UserService {
  @RequiresPermissions("admin")
  def addUser(user: User): Future[Unit]
}
```

We'd like the server to validate those permissions before passing the call to real RPC implementation.
Please recall one of the previous examples where we implemented a `RequestHandler` that received raw RPC invocations from the network and passed them to the real RPC. Below is a similar example for `UserService`. For simplicity, we're ignoring prodecures and getters and assume that only functions can be called.

```scala
import SimpleRPCFramework._
val realRpc: Services = ???
val rawRpc: RawRPC = AsRawRPC[UserService].asRaw(realRpc)

object SimpleHandler extends RequestHandler {
  // let's ignore `getterChain` and `handleFire`
  def handleCall(invocation: RawInvocation): Future[RawValue] =
    rawRpc.call(invocation.rpcName, invocation.argLists)
}
```

Now, let's add some permission validation. We'll assume having a method `validatePermissions` that checks if current user has all the necesary permissions to call the RPC.

```scala
import SimpleRPCFramework._
val realRpc: UserService = ???
val rawRpc: RawRPC = AsRawRPC[UserService].asRaw(realRpc)
val rpcMd = RPCMetadata[UserService]

object SimpleHandler extends RequestHandler {
  def validatePermissions(perms: Seq[String]): Boolean = ???

  // let's ignore `getterChain` and `handleFire`
  def handleCall(invocation: RawInvocation): Future[RawValue] = {
    val perms = rpcMd.signatures(invocation.rpcName).annotations
      .collectFirst({case RequiresPermissions(perms) => perms})
      .getOrElse(Nil)
    if(validatePermissions(perms)) rawRpc.call(invocation.rpcName, invocation.argLists)
    else Future.failed(new Exception("Forbidden!"))
  }
}
```

## Declaring typeclass instances explicitly

For every RPC trait, there are usually three typeclass instances needed: `MyRPCFramework.AsRawRPC`, `MyRPCFramework.AsRealRPC` and (optionally) `RPCMetadata`. Instances for all these typeclasses are automatically materialized by macros. If your trait is designed specifically for particular `RPCFramework` implementation then in order to avoid problems with incremental compilation and possible duplication of code generated by macros, it is recommended that these type class instances be declared explicitly in companion objects of your RPC traits. For example:

```scala
import com.avsystem.commons.rpc._

@RPC trait MyApi {
  ...
}
object MyApi {
  implicit val asRawRPC: MyRPCFramework.AsRawRPC[MyApi] = MyRPCFramework.materializeAsRaw[MyApi]
  implicit val asRealRPC: MyRPCFramework.AsRealRPC[MyApi] = MyRPCFramework.materializeAsReal[MyApi]
  implicit val rpcMetadata: RPCMetadata[MyApi] = RPCMetadata.materialize[MyApi]
}
```

There is also a convenient abstract class that lets you define companion object with all three implicits in one line:

```scala
import com.avsystem.commons.rpc._

@RPC trait MyApi {
  ...
}
object MyApi extends RPCTypeClasses[MyRPCFramework.type, MyApi]
```
