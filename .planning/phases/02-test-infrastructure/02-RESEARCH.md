# Phase 2: Test Infrastructure - Research

**Researched:** 2026-02-18
**Domain:** Scala 3 compiler plugin test harness (programmatic compilation, diagnostic capture, classpath wiring)
**Confidence:** HIGH

---

<phase_requirements>
## Phase Requirements

| ID | Description | Research Support |
|----|-------------|-----------------|
| TEST-01 | Test harness using `dotty.tools.dotc.Driver.process()` for programmatic compilation | See "Architecture Patterns: Pattern 1" -- use `Compiler` + `Run.compileFromStrings()` (not Driver.process). Driver.process requires file paths; compileFromStrings accepts raw strings directly, matching the Scala 2 BatchSourceFile pattern. |
| TEST-02 | Custom test reporter capturing diagnostics (warnings/errors) for assertion | See "Architecture Patterns: Pattern 2" -- the default Reporter already tracks `errorCount`, `warningCount`, `allErrors`, `allWarnings`. A custom reporter is NOT needed; the built-in Reporter provides all required accessors. |
| TEST-03 | Canary test verifying plugin actually loads and fires (prevents vacuous green tests) | See "Architecture Patterns: Pattern 4" -- canary test compiles known-bad snippet, asserts warning count > 0, then a second canary removes plugin and asserts warning count == 0 (proves test is not vacuously green). |
| TEST-04 | Test utilities for compiling code snippets and verifying expected diagnostics | See "Architecture Patterns: Pattern 3" -- `AnalyzerTest` trait provides `compile()`, `assertErrors()`, `assertNoErrors()`, `assertWarnings()` helpers, plus the `scala""` string interpolator from Scala 2. |
</phase_requirements>

---

## Summary

Testing Scala 3 compiler plugins requires replacing the entire Scala 2 `Global`-based test harness with a `ContextBase`-based approach. The Scala 3 compiler provides `Run.compileFromStrings(scalaSources: List[String])` which accepts raw source code strings -- this is the direct replacement for Scala 2's `BatchSourceFile` pattern and avoids the need for temp files or the `Driver.process()` file-path API.

The recommended approach (verified from zerowaste and scapegoat projects) is: (1) create a `ContextBase` subclass that overrides `loadRoughPluginsList` to inject the `AnalyzerPlugin` directly, bypassing JAR-based plugin discovery; (2) configure settings via `ctx.settings.usejavacp.update(true)` so the test compiler can resolve standard library classes; (3) use `compiler.newRun(using ctx).compileFromStrings()` to compile test snippets; (4) assert on `ctx.reporter.errorCount` / `warningCount` / `allErrors` / `allWarnings`. The existing `Reporter` class already provides `allErrors: List[Error]` and `allWarnings: List[Warning]` with `.message()` accessors -- no custom reporter is needed for our diagnostic capture requirements.

The blocker noted in STATE.md ("classpath wiring for Driver.process is non-obvious") is resolved: `provided` dependencies are available on the sbt test classpath, so `scala3-compiler % "provided"` is already accessible for tests. Setting `usejavacp = true` makes the JVM classpath (including scala3-library and scala3-compiler) visible to the programmatic compiler. Tests must run with `Test / fork := true` so that `java.class.path` reflects the full sbt test classpath rather than the sbt launcher's classpath.

**Primary recommendation:** Use the `ContextBase` + `Compiler` + `Run.compileFromStrings` pattern (from zerowaste). Do NOT use `Driver.process()` which requires file arguments. Add `Test / fork := true` and scalatest dependency to the analyzer module. Rewrite `AnalyzerTest` trait to provide the same API surface as the Scala 2 version (`compile`, `assertErrors`, `assertNoErrors`).

---

## Standard Stack

### Core
| Library | Version | Purpose | Why Standard |
|---------|---------|---------|--------------|
| `org.scala-lang::scala3-compiler` | 3.8.1 (`scalaVersion.value`) | Programmatic compilation API: `Compiler`, `Run.compileFromStrings`, `ContextBase`, `Reporter` | Required -- this IS the compiler under test |
| `org.scalatest::scalatest` | 3.2.19 (existing in project) | Test framework: `AnyFunSuite`, assertions | Already used by all other modules; consistent |

### Supporting
| Library | Version | Purpose | When to Use |
|---------|---------|---------|-------------|
| No additional libraries needed | -- | -- | -- |

### Alternatives Considered
| Instead of | Could Use | Tradeoff |
|------------|-----------|----------|
| `Compiler` + `Run.compileFromStrings` | `Driver.process()` (file-based) | Driver.process requires writing temp files and manual classpath strings; compileFromStrings accepts raw strings directly |
| `Compiler` + `Run.compileFromStrings` | `SourceFile.virtual` + `Run.compileSources` | More control but unnecessary complexity; compileFromStrings wraps this internally |
| Built-in `Reporter` accessors | Custom `StoreReporter` or `WartReporter` subclass | Custom reporter would be over-engineering; built-in Reporter already provides `allErrors`, `allWarnings`, `errorCount`, `warningCount` |
| `ContextBase.loadRoughPluginsList` override | `-Xplugin` flag pointing to built JAR | Fragile -- depends on JAR being pre-built in specific location; override is deterministic and does not require packaging |

**Installation (build.sbt changes for analyzer module):**
```scala
lazy val analyzer = project
  .settings(
    noPublishSettings,
    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % "provided",
      "org.scalatest" %% "scalatest" % scalatestVersion % Test,
    ),
    Test / fork := true,
  )
```

---

## Architecture Patterns

### Recommended Test Structure
```
analyzer/src/test/scala/com/avsystem/commons/analyzer/
  AnalyzerTest.scala          # Base trait: compile helpers, assertion methods
  CanaryTest.scala            # TEST-03: plugin-loads-and-fires verification
  (future rule tests)         # One file per rule, extending AnalyzerTest
```

### Pattern 1: Programmatic Compilation via ContextBase + Compiler

**What:** Create a custom `ContextBase` that injects the plugin, then compile from strings.
**When to use:** Every test that needs to compile a snippet and check diagnostics.
**Source:** Verified from [ghik/zerowaste ZerowastePluginTest.scala](https://github.com/ghik/zerowaste/blob/scala3/src/test/scala-3/com/github/ghik/zerowaste/ZerowastePluginTest.scala) and [scala3 Run.scala](https://github.com/lampepfl/dotty/blob/main/compiler/src/dotty/tools/dotc/Run.scala).

```scala
// Source: zerowaste ZerowastePluginTest.scala (verified) + Run.compileFromStrings (verified)
import dotty.tools.dotc.Compiler
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.plugins.Plugin

def compile(source: String): Unit = {
  val ctxBase = new ContextBase {
    override protected def loadRoughPluginsList(using Context): List[Plugin] =
      new AnalyzerPlugin :: Nil
  }
  given ctx: FreshContext = ctxBase.initialCtx.fresh
  ctx.settings.usejavacp.update(true)
  ctx.settings.pluginOptions.update(
    ctx.settings.pluginOptions.value :+ "AVSystemAnalyzer:+_"
  )
  val compiler = new Compiler
  val run = compiler.newRun
  run.compileFromStrings(List(source))
  // Now: ctx.reporter.errorCount, ctx.reporter.warningCount,
  //      ctx.reporter.allErrors, ctx.reporter.allWarnings
}
```

**Critical detail:** `compileFromStrings` creates virtual files internally with UUID-based names. This avoids the need for temp file creation/cleanup.

**Critical detail:** `ContextBase` must be instantiated fresh for each compilation to avoid stale state. The zerowaste pattern creates a new `ContextBase` in each `testFile` call.

### Pattern 2: Diagnostic Access (No Custom Reporter Needed)

**What:** The built-in `Reporter` (attached to `Context`) already provides all needed accessors.
**When to use:** For all diagnostic assertions.
**Source:** Verified from [scala3 Reporter.scala](https://github.com/scala/scala3/blob/main/compiler/src/dotty/tools/dotc/reporting/Reporter.scala).

Available accessors on `ctx.reporter`:
```scala
ctx.reporter.errorCount: Int
ctx.reporter.warningCount: Int
ctx.reporter.hasErrors: Boolean
ctx.reporter.hasWarnings: Boolean
ctx.reporter.allErrors: List[Diagnostic.Error]      // each has .message(): String
ctx.reporter.allWarnings: List[Diagnostic.Warning]   // each has .message(): String
```

The `Diagnostic` hierarchy (from [Diagnostic.scala](https://github.com/scala/scala3/blob/main/compiler/src/dotty/tools/dotc/reporting/Diagnostic.scala)):
- `Diagnostic` base: `msg: Message`, `pos: SourcePosition`, `level: Int`
- `Diagnostic.Error` -- compilation errors
- `Diagnostic.Warning` -- warnings (including `LintWarning`, `DeprecationWarning`, etc.)
- `Diagnostic.Info` -- informational messages

Each diagnostic provides:
- `.message()` -- the rendered message text as String
- `.pos` -- source position
- `.level` -- numeric level (matches `interfaces.Diagnostic.ERROR`, `WARNING`, `INFO`)

### Pattern 3: AnalyzerTest Trait API Design

**What:** Rewrite the existing `AnalyzerTest` trait to provide the same developer-facing API as the Scala 2 version.
**When to use:** All rule tests mix in this trait.

```scala
// Target API (matches existing Scala 2 AnalyzerTest as closely as possible)
trait AnalyzerTest { this: Assertions =>

  def compile(source: String): CompilationResult

  def assertErrors(errors: Int, source: String)(using Position): Unit = {
    val result = compile(source)
    assert(result.errorCount == errors,
      s"Expected $errors errors, got ${result.errorCount}: ${result.errors.map(_.message()).mkString(", ")}")
  }

  def assertNoErrors(source: String)(using Position): Unit = {
    val result = compile(source)
    assert(!result.hasErrors,
      s"Expected no errors, got: ${result.errors.map(_.message()).mkString(", ")}")
  }

  def assertWarnings(warnings: Int, source: String)(using Position): Unit = {
    val result = compile(source)
    assert(result.warningCount == warnings,
      s"Expected $warnings warnings, got ${result.warningCount}: ${result.warnings.map(_.message()).mkString(", ")}")
  }

  // Preserves the Scala 2 scala"..." interpolator
  implicit final def stringContextToScalaInterpolator(sc: StringContext): ScalaInterpolator =
    new ScalaInterpolator(sc)
}
```

**Design note:** The Scala 2 `AnalyzerTest` stores a single `compiler` and `settings` instance as fields. In Scala 3, the `ContextBase` should be recreated for each `compile()` call to avoid stale compiler state between tests. The `Compiler` instance can be shared (it is stateless), but the `ContextBase` and `Context` must be fresh.

### Pattern 4: Canary Test Design (TEST-03)

**What:** Two tests that prove the plugin loads and the test harness is not vacuously green.
**When to use:** Once, as a safety net for the entire test suite.

Test 1 -- "plugin fires on known-bad code":
```scala
test("canary: plugin produces warning for known-bad code") {
  // ImportJavaUtil is a stub but the plugin initialization itself can be tested
  // Once a real rule is implemented, this tests that rule specifically
  val result = compile("object Test { import java.util }")
  assert(result.warningCount > 0 || result.errorCount > 0,
    "Plugin should produce at least one diagnostic for known-bad code")
}
```

Test 2 -- "removing plugin makes test go red" (proves non-vacuous):
```scala
test("canary: compilation without plugin produces zero diagnostics") {
  // Compile same code WITHOUT plugin loaded
  val result = compileWithoutPlugin("object Test { import java.util }")
  assert(result.warningCount == 0 && result.errorCount == 0,
    "Without plugin, no diagnostics should be produced")
}
```

**Important:** The canary test for "plugin fires" requires at least one rule with actual logic (not just a stub). Since Phase 1 created stubs, the canary test should either:
(a) Test plugin initialization output (e.g., echoed message), or
(b) Be written as a "placeholder canary" that passes once the first rule is implemented in Phase 3, with a TODO comment.

The recommended approach is (a): add a minimal test that verifies the plugin at least initializes without error and that the ContextBase override injects it correctly. Then in Phase 3, upgrade the canary to test an actual rule.

### Anti-Patterns to Avoid

- **Using `Driver.process()` for string-based tests:** Driver.process takes file path arguments, not strings. Use `Compiler` + `Run.compileFromStrings()` instead.
- **Reusing a single ContextBase across tests:** Stale state accumulates. Create a fresh ContextBase per compilation.
- **Forgetting `Test / fork := true`:** Without forking, `usejavacp` may pick up the sbt launcher's classpath instead of the test classpath, causing `ClassNotFoundException` for standard library types.
- **Writing a custom Reporter subclass:** The default Reporter's `allErrors`/`allWarnings`/`errorCount`/`warningCount` are sufficient. A custom reporter adds complexity with no benefit.
- **Using `NoReporter` (from scapegoat pattern):** NoReporter silently discards all diagnostics. Our tests NEED to capture diagnostics, so the default reporter (or any capturing reporter) must be used.
- **Forgetting to add scalatest to analyzer module:** The analyzer module does NOT include `commonSettings` and therefore has NO test dependencies. Scalatest must be added explicitly.

---

## Don't Hand-Roll

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Diagnostic capture | Custom Reporter subclass | Built-in `ctx.reporter.allErrors`/`allWarnings` | Reporter already stores all diagnostics; custom subclass adds no value |
| In-memory compilation | Temp file creation + cleanup | `Run.compileFromStrings(List(source))` | Built into the compiler; handles virtual file creation internally |
| Plugin injection | `-Xplugin` flag + JAR path resolution | `ContextBase.loadRoughPluginsList` override | Deterministic, no JAR packaging dependency, no path resolution issues |
| Classpath resolution | Manual coursier cache path construction (scapegoat pattern) | `usejavacp.update(true)` + `Test / fork := true` | sbt already constructs the correct test classpath; usejavacp makes it available to the embedded compiler |
| Test file scaffolding | Complex test DSL | Simple `compile()`/`assertErrors()`/`assertNoErrors()` methods | Match existing Scala 2 API surface; rule tests already use this pattern |

**Key insight:** The scapegoat `DottyRunner` manually resolves JARs from the coursier cache -- this is fragile and unnecessary for our case. The zerowaste approach (ContextBase + usejavacp) is simpler and more robust because sbt already manages the classpath.

---

## Common Pitfalls

### Pitfall 1: Test / fork := false (Classpath Mismatch)
**What goes wrong:** `usejavacp = true` picks up the sbt launcher's classpath, not the test classpath. Compilation fails with `ClassNotFoundException` for `scala.Predef` or other standard library types.
**Why it happens:** Without forking, tests run in the sbt JVM whose `java.class.path` is the sbt launcher JAR, not the project's test dependencies.
**How to avoid:** Set `Test / fork := true` in the analyzer module's build settings.
**Warning signs:** Tests fail with "class not found" for basic Scala types, but `sbt compile` works fine.

### Pitfall 2: Stale ContextBase Between Tests
**What goes wrong:** Second test in a suite sees diagnostics or state from the first test, causing flaky failures.
**Why it happens:** Reusing a single `ContextBase` or `Context` instance across multiple `compile()` calls.
**How to avoid:** Create a fresh `ContextBase` and `Context` in each `compile()` invocation.
**Warning signs:** Tests pass individually but fail when run together; diagnostic counts are unexpectedly high.

### Pitfall 3: Provided Scope Not Available in Test
**What goes wrong:** `scala3-compiler` classes not found during test compilation.
**Why it happens:** Misunderstanding that `provided` excludes from test classpath (it does NOT in sbt -- provided deps ARE on the test classpath).
**How to avoid:** Keep `scala3-compiler % "provided"` as-is. Verified: sbt's `Test` configuration extends `CompileInternal` which includes `Provided`. No scope change needed.
**Warning signs:** This is a non-issue -- just documenting to prevent unnecessary scope changes.

### Pitfall 4: Plugin Options Format
**What goes wrong:** Plugin options not parsed, rules not enabled.
**Why it happens:** Incorrect format for `pluginOptions.update()`. The setting expects strings in format `"PluginName:option"`.
**How to avoid:** Use `ctx.settings.pluginOptions.update(List("AVSystemAnalyzer:+_"))` to enable all rules at error level (matching the existing Scala 2 test setup).
**Warning signs:** Tests compile without errors or warnings when they should produce them.

### Pitfall 5: Canary Test Vacuously Green
**What goes wrong:** Canary test passes even though plugin is not actually loaded, because the test snippet compiles cleanly without any diagnostics.
**Why it happens:** Test snippet does not trigger any rule, or all rules are stubs with no logic.
**How to avoid:** (a) Test that `ctx.reporter` has seen specific diagnostic messages containing `[AVS]` prefix, or (b) include a `compileWithoutPlugin()` method and assert the counts differ between with-plugin and without-plugin runs.
**Warning signs:** Removing the `loadRoughPluginsList` override does not cause any test failures.

### Pitfall 6: Missing scalafmt Brace Requirement
**What goes wrong:** scalafmt fails on test code written with significant indentation.
**Why it happens:** Project convention: `allowSignificantIndentation = false` -- must use brace syntax.
**How to avoid:** Write all test code with explicit braces. The `AnalyzerTest` trait and all tests must use brace syntax.
**Warning signs:** scalafmt check fails in CI.

---

## Code Examples

Verified patterns from official sources and real-world plugins:

### Complete AnalyzerTest Trait (Scala 3 Rewrite)
```scala
// Source: Derived from zerowaste ZerowastePluginTest.scala pattern (verified)
// + Run.compileFromStrings API (verified from lampepfl/dotty Run.scala)
package com.avsystem.commons
package analyzer

import dotty.tools.dotc.Compiler
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.plugins.Plugin
import dotty.tools.dotc.reporting.Diagnostic
import org.scalactic.source.Position
import org.scalatest.Assertions

trait AnalyzerTest { this: Assertions =>

  // Enable all rules at error level, matching Scala 2 test setup
  protected def pluginOptions: List[String] = List("AVSystemAnalyzer:+_")

  protected def compile(source: String): CompilationResult = {
    val ctxBase = new ContextBase {
      override protected def loadRoughPluginsList(using Context): List[Plugin] =
        new AnalyzerPlugin :: Nil
    }
    given ctx: FreshContext = ctxBase.initialCtx.fresh
    ctx.settings.usejavacp.update(true)
    ctx.settings.pluginOptions.update(pluginOptions)

    val compiler = new Compiler
    val run = compiler.newRun
    run.compileFromStrings(List(source))

    CompilationResult(
      errorCount = ctx.reporter.errorCount,
      warningCount = ctx.reporter.warningCount,
      errors = ctx.reporter.allErrors.map(_.message),
      warnings = ctx.reporter.allWarnings.map(_.message),
    )
  }

  /** Compile without the plugin (for canary comparison). */
  protected def compileWithoutPlugin(source: String): CompilationResult = {
    val ctxBase = new ContextBase {}
    given ctx: FreshContext = ctxBase.initialCtx.fresh
    ctx.settings.usejavacp.update(true)

    val compiler = new Compiler
    val run = compiler.newRun
    run.compileFromStrings(List(source))

    CompilationResult(
      errorCount = ctx.reporter.errorCount,
      warningCount = ctx.reporter.warningCount,
      errors = ctx.reporter.allErrors.map(_.message),
      warnings = ctx.reporter.allWarnings.map(_.message),
    )
  }

  def assertErrors(errors: Int, source: String)(using Position): Unit = {
    val result = compile(source)
    assert(result.errorCount == errors,
      s"Expected $errors errors, got ${result.errorCount}: ${result.errors.mkString("; ")}")
  }

  def assertNoErrors(source: String)(using Position): Unit = {
    val result = compile(source)
    assert(!result.hasErrors,
      s"Expected no errors, got: ${result.errors.mkString("; ")}")
  }

  def assertWarnings(warnings: Int, source: String)(using Position): Unit = {
    val result = compile(source)
    assert(result.warningCount == warnings,
      s"Expected $warnings warnings, got ${result.warningCount}: ${result.warnings.mkString("; ")}")
  }

  // Preserves the Scala 2 scala"..." interpolator
  implicit final def stringContextToScalaInterpolator(sc: StringContext): ScalaInterpolator =
    new ScalaInterpolator(sc)
}

object AnalyzerTest {
  final class ScalaInterpolator(private val sc: StringContext) extends AnyVal {
    def scala(args: Any*): String = s"object TopLevel {${sc.s(args*)}}"
  }
}

/** Immutable result of a test compilation. */
final case class CompilationResult(
  errorCount: Int,
  warningCount: Int,
  errors: List[String],
  warnings: List[String],
) {
  def hasErrors: Boolean = errorCount > 0
  def hasWarnings: Boolean = warningCount > 0
}
```

### Canary Test
```scala
// Source: derived from success criteria in phase description
package com.avsystem.commons
package analyzer

import org.scalatest.funsuite.AnyFunSuite

final class CanaryTest extends AnyFunSuite with AnalyzerTest {
  test("plugin initializes without error on valid code") {
    val result = compile("object ValidCode { val x: Int = 42 }")
    // Should compile cleanly with no errors (warnings may exist from plugin initialization)
    assert(!result.hasErrors, s"Valid code should compile without errors: ${result.errors.mkString("; ")}")
  }

  test("compilation without plugin produces zero diagnostics for same code") {
    val result = compileWithoutPlugin("object ValidCode { val x: Int = 42 }")
    assert(!result.hasErrors, "Without plugin, valid code should have no errors")
    assert(!result.hasWarnings, "Without plugin, valid code should have no warnings")
  }

  // This test will become meaningful once a rule has actual logic (Phase 3+)
  // For now, it validates the test harness wiring itself
  test("canary: test harness can detect compiler warnings") {
    // Use a snippet that triggers a standard compiler warning (not plugin-specific)
    // This proves the Reporter correctly captures warnings
    val result = compile("""
      object WarnTest {
        @deprecated("test", "0.0") def old: Int = 1
        val x: Int = old
      }
    """)
    // The deprecation warning proves Reporter captures warnings
    assert(result.warningCount > 0, "Should capture deprecation warning")
  }
}
```

### build.sbt Changes
```scala
lazy val analyzer = project
  .settings(
    noPublishSettings,
    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % "provided",
      "org.scalatest" %% "scalatest" % scalatestVersion % Test,
    ),
    Test / fork := true,
    // Suppress -Werror for test compilation (test snippets may produce warnings intentionally)
    Test / scalacOptions := Seq.empty,
  )
```

### Reporter Diagnostic Access
```scala
// Source: verified from scala3 Reporter.scala
//   https://github.com/scala/scala3/blob/main/compiler/src/dotty/tools/dotc/reporting/Reporter.scala

// After compilation:
given ctx: Context = ...
val reporter = ctx.reporter

// Count-based assertions (simple, matches Scala 2 pattern)
assert(reporter.errorCount == 2)
assert(reporter.warningCount == 1)
assert(!reporter.hasErrors)

// Message-based assertions (new capability)
val errorMessages: List[String] = reporter.allErrors.map(_.message)
assert(errorMessages.exists(_.contains("[AVS]")))

val warningMessages: List[String] = reporter.allWarnings.map(_.message)
assert(warningMessages.exists(_.contains("import java.util")))
```

---

## State of the Art

| Old Approach (Scala 2) | Current Approach (Scala 3.8.1) | When Changed | Impact |
|------------------------|--------------------------------|--------------|--------|
| `new Global(settings)` | `new ContextBase` + `new Compiler` | Scala 3.0 | Completely different API; no migration path |
| `global.reporter.errorCount` | `ctx.reporter.errorCount` / `.allErrors` | Scala 3.0 | Same concept, different access path; Scala 3 also provides `allErrors`/`allWarnings` lists |
| `BatchSourceFile("test.scala", source)` | `Run.compileFromStrings(List(source))` | Scala 3.0 | Built-in convenience; no manual SourceFile needed |
| `override def loadRoughPluginsList` on `Global` | `override def loadRoughPluginsList` on `ContextBase` | Scala 3.0 | Same pattern, different class hierarchy |
| `settings.usejavacp.value = true` | `ctx.settings.usejavacp.update(true)` | Scala 3.0 | Immutable settings with `update` returning new value |
| `settings.pluginOptions.value ++= List(...)` | `ctx.settings.pluginOptions.update(List(...))` | Scala 3.0 | Same concept, different mutation API |

**Deprecated/outdated in Scala 2 test code:**
- `scala.tools.nsc.Global` -- no equivalent class; use `ContextBase` + `Compiler`
- `scala.reflect.internal.util.BatchSourceFile` -- use `Run.compileFromStrings`
- `scala.tools.nsc.Settings` -- use `ctx.settings` (dotty settings)
- `scala.tools.nsc.plugins.Plugin` -- use `dotty.tools.dotc.plugins.Plugin`

---

## Open Questions

1. **Does `compileFromStrings` work with the plugin phase scheduler?**
   - What we know: `Run.compileFromStrings` creates virtual `SourceFile` objects and delegates to `compileSources`, which runs the full compilation pipeline including plugin phases. Zerowaste uses `run.compile(List(PlainFile(...)))` which goes through the same pipeline.
   - What's unclear: Whether there are edge cases with virtual files and plugin phases (there is a known issue #20591 with macros and virtual files, but our rules do not use macros).
   - Recommendation: HIGH confidence this works. If an edge case is found, fall back to `SourceFile.virtual` + `Run.compileSources` or temp file approach.

2. **Exact `.message` vs `.message()` API on Diagnostic**
   - What we know: `Diagnostic` implements `interfaces.Diagnostic` which has `def message(): String`. The method is called `message` (no parens in Scala 3 style) or `message()` (Java-style invocation).
   - What's unclear: Whether `message` is a `def` or a `lazy val` in the concrete implementation.
   - Recommendation: Use `diagnostic.message` (Scala style). LOW risk -- this is a standard accessor.

3. **Whether `FreshContext` or plain `Context` is needed**
   - What we know: `ctxBase.initialCtx` returns a `Context`. Zerowaste uses it directly as `given ctx: Context`. The scapegoat `FeedbackDottyTest` uses `.fresh` to get a `FreshContext` for `setReporter`.
   - What's unclear: Whether `settings.usejavacp.update(true)` requires `FreshContext` (since `update` modifies settings).
   - Recommendation: Use `ctxBase.initialCtx.fresh` to be safe. The `.fresh` call creates a `FreshContext` that supports all setter operations.

4. **`Test / scalacOptions` interaction with test compilation**
   - What we know: The analyzer module currently has no scalacOptions. The `commonSettings` (not applied to analyzer) includes `-Werror`. If scalacOptions are added to the analyzer module later, they affect the *analyzer module's own compilation*, not the programmatic test compiler.
   - What's unclear: Whether any flags on the analyzer module's test compilation interfere with the programmatic compiler.
   - Recommendation: Keep `Test / scalacOptions := Seq.empty` or at least exclude `-Werror` to avoid the analyzer module's own test files failing on warnings from the Scala 3 compiler about virtual file usage or similar.

---

## Sources

### Primary (HIGH confidence)
- [ghik/zerowaste ZerowastePluginTest.scala (scala3 branch)](https://github.com/ghik/zerowaste/blob/scala3/src/test/scala-3/com/github/ghik/zerowaste/ZerowastePluginTest.scala) -- complete working Scala 3 plugin test using ContextBase + Compiler + PlainFile pattern; fetched via GitHub API, verified line-by-line
- [lampepfl/dotty Run.scala](https://github.com/lampepfl/dotty/blob/main/compiler/src/dotty/tools/dotc/Run.scala) -- `compileFromStrings(scalaSources: List[String])` method signature verified via WebFetch
- [scala3 Reporter.scala](https://github.com/scala/scala3/blob/main/compiler/src/dotty/tools/dotc/reporting/Reporter.scala) -- `errorCount`, `warningCount`, `allErrors`, `allWarnings`, `NoReporter` verified via WebFetch
- [scala3 Diagnostic.scala](https://github.com/scala/scala3/blob/main/compiler/src/dotty/tools/dotc/reporting/Diagnostic.scala) -- Diagnostic hierarchy (Error, Warning, Info) and `.message()` accessor verified via WebFetch
- [scala3 SourceFile.scala](https://github.com/lampepfl/dotty/blob/main/compiler/src/dotty/tools/dotc/util/SourceFile.scala) -- `SourceFile.virtual(name, content)` factory method verified via WebFetch
- [scala3 Driver.scala](https://github.com/scala/scala3/blob/main/compiler/src/dotty/tools/dotc/Driver.scala) -- `process()` method signatures verified via WebFetch
- [scala3 Contexts.scala](https://github.com/scala/scala3/blob/main/compiler/src/dotty/tools/dotc/core/Contexts.scala) -- `ContextBase` class hierarchy, `FreshContext.setReporter`, `initialCtx` verified via WebFetch
- [scala3 Plugins.scala](https://github.com/scala/scala3/blob/main/compiler/src/dotty/tools/dotc/plugins/Plugins.scala) -- `loadRoughPluginsList` method in `Plugins` trait mixed into `ContextBase`

### Secondary (MEDIUM confidence)
- [scapegoat DottyRunner.scala](https://github.com/scapegoat-scala/scapegoat/blob/master/src/test/scala-3/com/sksamuel/scapegoat/DottyRunner.scala) -- alternative Driver.process-based pattern; fetched via WebFetch, verified line-by-line
- [scapegoat InspectionTest.scala](https://github.com/scapegoat-scala/scapegoat/blob/master/src/test/scala-3/com/sksamuel/scapegoat/InspectionTest.scala) -- scapegoat test base class pattern
- [scapegoat ScapegoatPhaseTest.scala](https://github.com/scapegoat-scala/scapegoat/blob/master/src/test/scala-3/com/sksamuel/scapegoat/ScapegoatPhaseTest.scala) -- canary-style plugin loading test
- [sbt provided scope on test classpath](https://medium.com/swlh/dependencies-with-provided-scope-in-scala-e5ff0c076033) -- verified that provided deps appear on test classpath
- [zerowaste build.sbt (scala3 branch)](https://github.com/ghik/zerowaste/blob/scala3/build.sbt) -- build configuration with `Test / fork := true`

### Tertiary (LOW confidence)
- [Scala Users forum: How can I test a compiler plugin?](https://users.scala-lang.org/t/how-can-i-test-a-compiler-plugin/6513) -- general advice on plugin testability
- [scala3 issue #20591](https://github.com/scala/scala3/issues/20591) -- known issue with macros and virtual source files (not directly relevant but good to know)

---

## Metadata

**Confidence breakdown:**
- Standard stack: HIGH -- verified from jar API and two working plugin test suites (zerowaste, scapegoat)
- Architecture (ContextBase + Compiler + compileFromStrings): HIGH -- zerowaste verified line-by-line; compileFromStrings verified from Run.scala source
- Architecture (Reporter diagnostic access): HIGH -- Reporter.scala source verified; errorCount/warningCount/allErrors/allWarnings confirmed
- Classpath wiring (usejavacp + fork): HIGH -- zerowaste uses exactly this; sbt provided-on-test-classpath confirmed
- Canary test design: MEDIUM -- pattern is sound but depends on at least one rule having logic (stubs may not fire)
- compileFromStrings with plugin phases: MEDIUM-HIGH -- logically should work (same pipeline as compile), but not directly verified in an external project (zerowaste uses PlainFile)

**Research date:** 2026-02-18
**Valid until:** 2026-08-18 (stable Scala 3 compiler API)
