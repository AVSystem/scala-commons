# AVSystem Commons Library for Scala

[![Build Status](https://travis-ci.org/AVSystem/scala-commons.svg?branch=master)](https://travis-ci.org/AVSystem/scala-commons)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.avsystem.commons/commons-core_2.11/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.avsystem.commons/commons-core_2.11)

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

## Features

`commons-shared` and `commons-core` modules:
* Java & Guava interoperability utilities
 * aliases for Java & Guava types
 * converters between Java and Scala collections (the same as in Scala standard library)
 * converters between Scala `Future`s and Guava `ListenableFuture`s
 * factory objects and `CanBuildFrom` instances for Java collections
 * factory methods for common Java functional interfaces
 * Scala API over Java 8 Stream API which accepts Scala lambdas
* shorter type aliases for Scala collections
* lightweight alternatives for Scala `Option`: `Opt`, `NOpt`, `OptRef` - implemented as value classes
* `GenCodec`: format-agnostic, typeclass based serialization framework with automatic derivation
* typesafe RPC/proxy framework
* utilities for sealed hierarchy based enums

`commons-macros` module:
* reusable macro/reflection utilities
* macro infrastructure for automatic type class derivation

`commons-spring` module:
* `BeanDefinitionReader` implementation for HOCON format

`commons-analyzer` module:
* custom static analyzer (a Scala compiler plugin)
