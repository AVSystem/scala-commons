# AVSystem Commons Library for Scala

[![Continuous Integration](https://github.com/AVSystem/scala-commons/actions/workflows/ci.yml/badge.svg)](https://github.com/AVSystem/scala-commons/actions/workflows/ci.yml)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.avsystem.commons/commons-core_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.avsystem.commons/commons-core_2.13)

**[API reference](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/index.html)**

**NOTE** This library is available to the public but it's mostly used internally at AVSystem. Therefore, API may frequently change in an incompatible way.

## Modules and features

* `commons-core` - basic language utilities and generic features not associated with any particular library of framework:
  * [`GenCodec`](docs/GenCodec.md): format-agnostic, typeclass based serialization framework with automatic typeclass
    derivation for case classes and sealed hierarchies
    * built-in serialization formats include JSON (raw string), [CBOR](docs/CBOR.md) and BSON (in `commons-mongo`).
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
    [`OptRef`](http://avsystem.github.io/scala-commons/api/com/avsystem/commons/misc/OptRef.html) (implemented as value
    classes)
  * [Components](docs/Components.md) and Dependency Injection library
* `commons-redis` - [Scala driver for Redis](docs/RedisDriver.md)
* `commons-macros` contains implementations of macros used in other modules and reusable macro utilities:
  * `MacroCommons` trait with several convenience functions for implementing macros
  * `TypeClassDerivation` - implements infrastructure for automatic type class derivation
* [`commons-analyzer`](docs/Analyzer.md) - static analyzer for Scala code, i.e. a compiler plugin that enforces various (mostly unrelated) rules and conventions on source code
* `commons-jetty` - Jetty server utilities
* [`commons-mongo`](docs/TypedMongo.md) - MongoDB utilities for Scala & Java MongoDB drivers, integration with `GenCodec`
* `commons-hocon` - Utilities for working with [HOCON](https://github.com/lightbend/config/blob/master/HOCON.md)
  * `HoconInput` - an `Input` implementation for `GenCodec` that can read Lightbend Config (`com.typesafe.config.Config`)
  * An AST (`HTree`) and a lexer/parser for HOCON (`HLexer`, `HParser`)
* `commons-spring` - Spring framework utilities:
  * `HoconBeanDefinitionReader` - an utility that allows you to define Spring application context using 
  [HOCON](https://github.com/lightbend/config/blob/master/HOCON.md) format
