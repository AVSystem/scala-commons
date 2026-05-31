# Scala 3 Migration Status

Tracks the Scala 3 cross-compile state of `AVSystem/scala-commons` on the `scala-3` branch. Source of truth for which modules cross-build on which Scala version, which carry MiMa baselines, and which deprecations remain to be resolved. Maintained PR-by-PR alongside the work it tracks.

## How to update

1. When a PR ports a module, flip that module's row in the Per-module status table in the same PR.
2. When a PR discovers a new `@deprecated` symbol, append it to the Deprecation log in the same PR.
3. MiMa and Tasty-MiMa columns change only after CI proves the new state green.
4. Commit messages and this document use upstream-facing terms only — no internal tooling vocabulary.
5. Internal planning artifacts never appear in PR diffs.

## Per-module status

Status tokens: `cross` (cross-builds on both versions), `stub` (Scala 3 wired with empty/placeholder sources), `2.13-only` (excluded from Scala 3 aggregate), `pending` (not yet started), `wip` (in progress on a feature branch). MiMa / Tasty-MiMa tokens: `green`, `red`, `n/a`, `pending`. Rows below `core` may be logical groupings, not separate sbt modules — see Notes.

| Module | 2.13 | 3.x | MiMa | Tasty-MiMa | Notes |
|--------|------|-----|------|------------|-------|
| macros | cross | stub | n/a | n/a | Whitebox 2.13 macros; Scala 3 jar empty in the next port. |
| made | n/a | pending | n/a | n/a | Scala-3-only dep, pinned to `io.github.halotukozak:made_3:0.1.0`. |
| core | cross | pending | green | pending | Cross-compile target; tests still pending on Scala 3. |
| hocon | cross | pending | green | pending | Pure-Scala; first downstream port after `core`. |
| mongo | cross | pending | green | pending | Uses `CrossVersion.for3Use2_13` wrapper on Scala 3. |
| mongo-js | cross | pending | n/a | n/a | ScalaJS variant of `mongo`. |
| core-js | cross | pending | n/a | n/a | ScalaJS variant of `core`. |
| benchmark3 | cross | pending | n/a | n/a | JMH benchmark module. |
| jetty | 2.13-only | n/a | n/a | n/a | Servlet/RPC churn; lives under `jvm2` aggregate. |
| analyzer | 2.13-only | n/a | n/a | n/a | Compiler plugin against scala-2 internals. |
| spring | 2.13-only | n/a | n/a | n/a | Deprecated upstream; no port planned. |
| RPC | 2.13-only | n/a | n/a | n/a | Logical concern under `core`; macro-stack-dependent. |
| cbor | cross | pending | pending | pending | Sub-package of `core`; tracked separately for MiMa scope. |

## 2.13-only modules

Modules in this group are excluded from the `jvm` aggregate (cross-built on both Scala versions) and live under the `jvm2` aggregate or remain unaggregated in `build.sbt`. Rationale per module follows.

### jetty

`jetty` integrates with the Servlet API and consumes the RPC framework. Both depend on the macro stack and on Servlet API versions that have not stabilized for Scala 3. The module stays under the `jvm2` aggregate so `++3.x jvm/test` skips it cleanly. Reactivation is deferred pending an RPC port decision (see also `RPC`).

### analyzer

`analyzer` is a Scala 2 compiler plugin that hooks into `scala.tools.nsc` internals. The Scala 3 compiler exposes a different plugin API; porting requires a rewrite against `dotty.tools.dotc.plugins` rather than a cross-build. Kept commented out in `build.sbt` and tracked here as `2.13-only`.

### spring

`spring` integrates with Spring Framework annotations. The module is deprecated upstream — no port to Scala 3 is planned. Status `2.13-only` reflects its position in the build; no future reactivation is expected.

### RPC

The RPC framework lives as a logical concern inside `core/src/main/scala-2.13/com/avsystem/commons/rpc/`. It is not a separate sbt module. Porting depends on the macro stack (see `macros`) and on the broader serialization / typeclass derivation port (see `core`). Tracked here so the deferral is explicit.

## Deprecation log

Seeded from a `@deprecated` scan against fork `master`. Populated in the same PR as this skeleton.
