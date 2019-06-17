# `commons-analyzer`

This module contains a simple static analyzer for Scala, implemented as a compiler plugin.

Here's how to configure it in `build.sbt`:

```scala
val avsCommonsVersion: String = ???
addCompilerPlugin("com.avsystem.commons" %% "commons-analyzer" % avsCommonsVersion)
```

Analyzer runs after typechecker inside the Scala compiler and applies its rules on every file.
Every rule can be disabled or enabled to yield a compilation error, warning or info.
It is recommended to use warnings with `-Xfatal-warning` option for the Scala compiler enabled
and [silencer](https://github.com/ghik/silencer) plugin for warning suppression.

Here's a list of currently supported rules:

| Name | Default level | Description |
| --- | --- | --- |
| `importJavaUtil` | warning | Rejects direct imports of `java.util` package. |
| `valueEnumExhaustiveMatch` | warning | Enables (limited) exhaustive pattern match checking for [`ValueEnum`s](https://github.com/AVSystem/scala-commons/blob/1.34.x/commons-core/src/main/scala/com/avsystem/commons/misc/ValueEnum.scala). |
| `any2stringadd` | off | Disables `any2stringadd` (concatenating arbitrary values with strings using `+`). |
| `bincompat` | warning | Enables [`@bincompat`](https://github.com/AVSystem/scala-commons/blob/1.34.x/commons-annotations/src/main/scala/com/avsystem/commons/annotation/bincompat.scala) checking |
| `showAst` | error | Handles [`@showAst`](https://github.com/AVSystem/scala-commons/blob/1.34.x/commons-annotations/src/main/scala/com/avsystem/commons/annotation/showAst.scala). |
| `findUsages` | warning | Issues a message for every occurrence of any of the symbols listed in the argument of this rule |
| `varargsAtLeast` | warning | Enables [`@atLeast`](https://github.com/AVSystem/scala-commons/blob/1.34.x/commons-annotations/src/main/scala/com/avsystem/commons/annotation/atLeast.scala) checking |
| `macroPrivate` | warning | Enables [`@macroPrivate`](https://github.com/AVSystem/scala-commons/blob/1.34.x/commons-annotations/src/main/scala/com/avsystem/commons/annotation/macroPrivate.scala) checking |
| `explicitGenerics` | warning | Enables [`@explicitGenerics`](https://github.com/AVSystem/scala-commons/blob/1.34.x/commons-annotations/src/main/scala/com/avsystem/commons/annotation/explicitGenerics.scala) checking |

Rules may be enabled and disabled in `build.sbt` with Scala compiler options:

```scala
scalacOptions += "-P:AVSystemAnalyzer:<level><ruleName>,<level><ruleName>,..."
```
`<name>` must be one of the rule names listed above or `_` to apply to all rules.

`<level>` may be one of:
* `-` to disable rule
* `*` for "info" level
* _empty_ for "warning" level
* `+` for "error" level
