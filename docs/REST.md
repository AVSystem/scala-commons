# REST framework

The commons libary contains an RPC based REST framework for defining REST services using Scala traits.
It may be used for implementing both client and server side and works in both JVM and JS, as long as
appropriate network layer is implemented. For JVM, Jetty-based implementations for client and server
are provided.

The core or REST framework is platform independent and network-implementation indepenedent and therefore
has no external dependencies. Because of that, it's a part of `commons-core` module. This is enough to
be able to define REST interfaces. But if you want to expose your REST interface through an actual HTTP
server or have an actual HTTP client for that interface, you need separate implementations for that.
The `commons-jetty` module automati

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
  @GET def getUsername(userId: String): Future[String]
}
object UserApi extends DefaultRestApiCompanion[UserApi]
```

Then, implement it on server side and expose it on localhost port 9090 using Jetty:

```scala
import com.avsystem.commons.jetty.rest.RestHandler
import org.eclipse.jetty.server.Server

class UserApiImpl extends UserApi {
  def getUsername(userId: String) = Future.successful(s"$userId-name")
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

    val result = proxy.getUsername("ID")
      .andThen({ case _ => client.stop() })
      .andThen {
        case Success(name) => println(s"Hello, $name!")
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
GET http://localhost:9090/getUsername?userId=ID HTTP/1.1
Accept-Encoding: gzip
User-Agent: Jetty/9.3.23.v20180228
Host: localhost:9090
```

Response:
```
HTTP/1.1 200 OK
Date: Wed, 11 Jul 2018 12:54:21 GMT
Content-Type: application/json;charset=utf-8
Content-Length: 9
Server: Jetty(9.3.23.v20180228)

"ID-name"
```
