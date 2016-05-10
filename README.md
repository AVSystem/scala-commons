# AVSystem Commons Library for Scala

[![Build Status](https://travis-ci.org/AVSystem/scala-commons.svg?branch=master)](https://travis-ci.org/AVSystem/scala-commons)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.avsystem.commons/commons-core_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.avsystem.commons/commons-core_2.11)

## Modules and features

* `commons-shared` - basic language utilities and generic features not associated with any particular library of framework:
 * Java interoperability utilities
 * Various Scala language-level utilities
 * Lightweight alternatives for Scala `Option` - `Opt`, `NOpt`, `OptRef` (implemented as value classes)
 * [`GenCodec`: format-agnostic, typeclass based serialization framework with automatic derivation](docs/GenCodec.md)
 * [Typesafe RPC/proxy framework](docs/RPCFramework.md)
 
 `commons-shared` module is cross compiled for JVM and JS.
* `commons-core` - additional language utilities available only on JVM
 * Java 8 interoperability utilities
 * Google Guava interoperability utilities
* `commons-macros` contains implementations of macros used in other modules and reusable macro utilities:
 * `MacroCommons` trait with several convenience functions for implementing macros
 * `TypeClassDerivation` - implements infrastructure for automatic type class derivation
* `commons-analyzer` - static analyzer for Scala code, i.e. a compiler plugin that enforces various (mostly undrelated) rules and conventions on source code
* `commons-annotations` - contains annotations used by macros and static analyzer
* `commons-jetty` - Jetty server utilities
* `commons-mongo` - MongoDB utilities
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
