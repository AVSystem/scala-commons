# Components and Dependency Injection Library

`commons-core` module contains a small sub-library for application components initialization and dependency
injection, featuring:

* defining application components in plain Scala (e.g. constructor invocations)
* automatic figuring out of dependency graph and initialization order
* initialization of independent components **in parallel**
* dependency injection using Scala implicits (compile-time "autowiring", opt-in)

## Defining components

An application is made of _components_. In this tutorial, a _component_ is an object 
that usually has at least one of the following properties:

* it represents a "service" within your system, e.g. HTTP server, database access layer, etc.
* it has some initialization logic with side effects
* it depends on other components


Components are defined in an object or class that extends `Components` trait which provides `component`
and `singleton` methods (macros) which interpret a plain Scala expression (e.g. constructor invocation) into
a `Component` definition.

```scala
import scala.concurrent._
import scala.concurrent.duration._
import com.avsystem.commons.di._

class Database {
  // DB initialization goes here...
}
class Server(database: Database) {
  // server initialization goes here...
  
  def join(): Unit = { /* wait for shutdown */ }
}

object MyApp extends Components {
  val server: Component[Server] = 
    component(new Server(database.ref))

  val database: Component[Database] =
    component(new Database)
    
  def main(args: Array[String]): Unit = {
    // use appropriate execution context of your choice for application initialization
    import ExecutionContext.Implicits.global
    val srv = Await.result(server.init, Duration.Inf)
    srv.join()
  }
}
```

## `Component`

The `Component` type can be summarized as an enhanced version of Scala's built-in `lazy val` construct.
It is associated with a lazy-evaluated expression that creates and initializes the component (often a 
simple constructor invocation, but it may be an arbitrary piece of code). 

Wrapping component definitions into `Component` instances relieves the programmer from manually figuring out 
the initialization order of multiple interdependent components. Note that simply using `lazy val`s would already
do that job, but `Component` does this better because:

* in case of failures, exceptions are wrapped in a way that lets you track the initialization path that
  leads to the faulty component (i.e. a stack trace but nicer to read than the raw one)
* dependency cycles are detected before any initialization is done - a nice error message is displayed that lets
  you quickly see where the cycle is
  
`Component` instances are aware of their source position. If they are assigned to a `val` or `def`, the name of
this `val` or `def` is interpreted as the component name. This name may be later useful for debugging. 
Here's an example of a dependency cycle error that uses component names and source positions:

```
Exception in thread "main" com.avsystem.commons.di.DependencyCycleException: component dependency cycle detected:
  server(MyApp.scala:13) ->
  service(MyApp.scala:16) ->
  database(MyApp.scala:19) ->
  server(MyApp.scala:13)
```

## Entry points

In order to start our application in the previous example, we explicitly requested initialization of the 
`server` component. This way we assumed `server` to be an _entry point_ of our application - the primary 
service that exposes core system functionality (e.g. an HTTP server).
All the other components are initialized only as direct or indirect dependencies of this toplevel entry point.

If your application consists of multiple such entry points, e.g. an UI server, REST API server, FTP server, etc.
then it is recommended to create a single toplevel component whose purpose is solely to aggregate all the 
"real" entry points into one.

```scala
import com.avsystem.commons.di._

class UiServer
class ApiServer
class FtpServer

class FullApplication(ui: UiServer, api: ApiServer, ftp: FtpServer)

object MyApp extends Components {
  val ui: Component[UiServer] = component(new UiServer)
  val api: Component[ApiServer] = component(new ApiServer)
  val ftp: Component[FtpServer] = component(new FtpServer)
  
  val fullApplication: Component[FullApplication] =
    component(new FullApplication(ui.ref, api.ref, ftp.ref))
}
```

This way your application always has a single entry point component which makes it easier to navigate
through code and understand its structure (knowing where to start).

## Dependency references

A `Component` may refer to another `Component` using `.ref`, e.g.

```scala
class Database
class Server(db: Database)

object MyApp extends Components {
  val database: Component[Database] = component(new Database)
  val server: Component[Server] = component(new Server(database.ref))
}
```

`.ref` is not a real method. It exists only during compilation and is interpreted by the `component` macro.
A dependency reference is extracted by the macro out of the component initialization expression. This way
the macro separates initialization of dependencies from initialization of the component itself.
This technique makes it possible to inspect the dependency graph before any components are initialized.
This in turn allows early detection of cycles and makes it possible to initialize independent components
in parallel.

## Implicit dependency injection

