# `commons-analyzer`

This module contains a simple static analyzer for Scala, implemented as a compiler plugin.

Here's how to configure it in `build.sbt`:

```scala
val avsCommonsVersion: String = ???
addCompilerPlugin("com.avsystem.commons" %% "commons-analyzer" % avsCommonsVersion)
```

Analyzer runs after typechecker inside the Scala compiler and applies its rules on every file.
Every rule can be disabled or enabled to yield a compilation error, warning or info.
It is recommended to use warnings with `-Werror` option for the Scala compiler enabled
and `@nowarn` annotation for warning suppression.

Here's a list of currently supported rules:

| Name                       | Default level | Description                                                                                                                                                                                                                       |
|----------------------------|---------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `importJavaUtil`           | warning       | Rejects direct imports of `java.util` package.                                                                                                                                                                                    |
| `valueEnumExhaustiveMatch` | warning       | Enables (limited) exhaustive pattern match checking for [`ValueEnum`s](https://github.com/AVSystem/scala-commons/blob/master/commons-core/src/main/scala/com/avsystem/commons/misc/ValueEnum.scala).                              |
| `any2stringadd`            | off           | Disables `any2stringadd` (concatenating arbitrary values with strings using `+`).                                                                                                                                                 |
| `bincompat`                | warning       | Enables [`@bincompat`](https://github.com/AVSystem/scala-commons/blob/master/commons-core/src/main/scala/com/avsystem/commons/annotation/bincompat.scala) checking                                                                |
| `showAst`                  | error         | Handles [`@showAst`](https://github.com/AVSystem/scala-commons/blob/master/commons-core/src/main/scala/com/avsystem/commons/annotation/showAst.scala).                                                                            |
| `findUsages`               | warning       | Issues a message for every occurrence of any of the symbols listed in the argument of this rule                                                                                                                                   |
| `varargsAtLeast`           | warning       | Enables [`@atLeast`](https://github.com/AVSystem/scala-commons/blob/master/commons-core/src/main/scala/com/avsystem/commons/annotation/atLeast.scala) checking                                                                    |
| `macroPrivate`             | warning       | Enables [`@macroPrivate`](https://github.com/AVSystem/scala-commons/blob/master/commons-core/src/main/scala/com/avsystem/commons/annotation/macroPrivate.scala) checking                                                          |
| `explicitGenerics`         | warning       | Enables [`@explicitGenerics`](https://github.com/AVSystem/scala-commons/blob/master/commons-core/src/main/scala/com/avsystem/commons/annotation/explicitGenerics.scala) checking                                                  |
| `discardedMonixTask`       | warning       | Makes sure that expressions evaluating to Monix `Task`s are not accidentally discarded by conversion to `Unit`                                                                                                                    |
| `throwableObjects`         | warning       | Makes sure that objects are never used as `Throwable`s (unless they have stack traces disabled)                                                                                                                                   |
| `constantDeclarations`     | off           | Checks if constants are always declared as `final val`s with `UpperCamelCase` and no explicit type annotation for literal values                                                                                                  |
| `basePackage`              | warning       | Checks if all sources are within the specified base package                                                                                                                                                                       |
| `catchThrowable`           | warning       | Makes sure that code does not catch `Throwable` directly, which can hide critical errors like `OutOfMemoryError`                                                                                                                  |
| `finalValueClasses`        | warning       | Makes sure that value classes are marked final.                                                                                                                                                                                   |
| `finalCaseClasses`         | warning       | Makes sure that case classes are marked final. Does not affect inner classes due to SI-4440                                                                                                                                       |
| `implicitParamDefaults`    | warning       | Makes sure that default values are not defined for implicit parameters                                                                                                                                                            |
| `implicitValueClasses`     | warning       | Makes sure that implicit classes extend `AnyVal` (when applicable). Available options: `top-level-only` (default) - only checks implicit classes in top-level objects; `all` - checks both top-level and nested implicit classes. |
| `implicitFunctionParams`   | warning       | Makes sure that implicit parameters are not function types or partial functions                                                                                                                                                   |

Rules may be enabled and disabled in `build.sbt` with Scala compiler options:

```scala
scalacOptions += "-P:AVSystemAnalyzer:<level><ruleName>[:<arg>],<level><ruleName>[:<arg>],..."
```

`<ruleName>` must be one of the rule names listed above or `_` to apply to all rules.

`<level>` may be one of:

* `-` to disable rule
* `*` for "info" level
* _empty_ for "warning" level
* `+` for "error" level

`<arg>` is an argument specific for given rule

For example, this options sets all rules on "error" level except for `constantDeclarations` which is disabled
and `discardedMonixTask` which is lowered to "warning" level. The `basePackage` rule is also lowered to "warning" level
and `com.avsystem.commons` is specified as the base package.

```scala
scalacOptions += "-P:AVSystemAnalyzer:+_,-constantDeclarations,discardedMonixTask,basePackage:com.avsystem.commons
```
