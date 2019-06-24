# AVSystem Commons Library for Scala

[![Build Status](https://travis-ci.org/AVSystem/scala-commons.svg?branch=master)](https://travis-ci.org/AVSystem/scala-commons)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.avsystem.commons/commons-core_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.avsystem.commons/commons-core_2.11)

**[API reference](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/index.html)**

**NOTE** This library is available to the public but it's mostly used internally at AVSystem. Therefore, API may frequently change in an incompatible way.

## Modules and features

* `commons-core` - basic language utilities and generic features not associated with any particular library of framework:
  * [`GenCodec`](docs/GenCodec.md): format-agnostic, typeclass based serialization framework with automatic typeclass
    derivation for case classes and sealed hierarchies
  * **Typesafe** RPC/proxy framework used in particular by [Udash Framework](http://guide.udash.io/#/rpc) for
    client-server communication
  * Better enumeration support for Scala -
    [`ValueEnum`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/misc/ValueEnum.html),
    [`SealedEnumCompanion`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/misc/SealedEnumCompanion.html),
    [`NamedEnumCompanion`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/misc/NamedEnumCompanion.html),
    [`OrderedEnum`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/misc/OrderedEnum.html)
  * Java interoperability utilities - [`JavaInterop`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/jiop/JavaInterop$.html)
  * Google Guava interoperability utilities (dependency on Guava is optional)
  * Various Scala language-level utilities
    * Implicit source information objects - [`SourceInfo`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/misc/SourceInfo.html)
    * More concise aliases for Scala collection types - [`CollectionAliases`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/collection/CollectionAliases$.html)
    * Universal extension methods - [`SharedExtensions`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/SharedExtensions$.html)
  * Lightweight alternatives for Scala `Option` -
    [`Opt`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/misc/Opt.html) - guarantees no `null`s,
    [`OptArg`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/misc/OptArg.html), 
    [`NOpt`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/misc/NOpt.html), 
    [`OptRef`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/misc/OptRef.html) (implemented as value classes)
* `commons-redis` - [Scala driver for Redis](docs/RedisDriver.md)
* `commons-akka` - Akka utilities, primarily [`AkkaRPCFramework`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/rpc/akka/AkkaRPCFramework$.html)
  which supports methods returning Monix `Observable`s
* `commons-macros` contains implementations of macros used in other modules and reusable macro utilities:
  * `MacroCommons` trait with several convenience functions for implementing macros
  * `TypeClassDerivation` - implements infrastructure for automatic type class derivation
* [`commons-analyzer`](docs/Analyzer.md) - static analyzer for Scala code, i.e. a compiler plugin that enforces various (mostly unrelated) rules and conventions on source code
* `commons-jetty` - Jetty server utilities
* `commons-mongo` - MongoDB utilities for Scala & Java MongoDB drivers, integration with `GenCodec`
* `commons-hocon` - Utilities for handling [HOCON](https://github.com/lightbend/config/blob/master/HOCON.md), e.g. a
  `HoconInput` implementation for `GenCodec`
* `commons-spring` - Spring framework utilities:
  * `HoconBeanDefinitionReader` - an utility that allows you to define Spring application context using HOCON format

## Development

* Introduce your changes
* Write some unit tests
* Bump the version number in `version.sbt` file
* Submit a pull request

### Test on local environment

* Publish the artifact locally
  * Use `sbt publishLocal` for projects managed by Ivy (e.g. SBT projects)
  * Or `sbt publishM2` for projects managed by maven
* Update avsystem-commons dependency version in your project and refresh
  your project dependencies
