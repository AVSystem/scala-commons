# Scala 3 Migration

This document tracks the state of the Scala 3 migration of `scala-commons`.
The codebase has pivoted to Scala 3 only. Features that didn't compile on Scala 3
are commented out under `// TODO[scala3-port]:` tags and listed in the backlog at
the bottom of this file. Restoration ships incrementally per feature area.

## 1. Will not migrate

| Symbol/module | Rationale |
|---------------|-----------|
| `commons-macros` module | Pure Scala 2 macro infrastructure (`c.universe`, blackbox/whitebox). No Scala 3 surface worth porting; Scala 3 uses inline + quotes which is a structural redesign. Module deleted outright in Phase 1. |
| `analyzer` module | Scala 2 compiler plugin; would require a full Scala 3 compiler plugin rewrite. Restored only if downstream demand surfaces. |
| `jetty` module | ee10 servlet wrapper; deprecated upstream. Out of scope until/unless restored. |
| `spring` module | spring-context wiring; deprecated upstream. Out of scope until/unless restored. |
| `comprof` module | `scalac-profiling` plugin is Scala 2 only. Retire pending a Scala 3 alternative. |
| `-Xsource:3` scalac flag | Obsolete — the codebase IS Scala 3 now. |
| `-Wconf` warning suppression blocks | Project rule: fix warnings at source, not via suppression. |
| `-language:experimental.macros` | Scala 3 uses `inline` + `scala.quoted`; flag is a no-op. |
| Scala 2 macro impls (`c.universe`, blackbox/whitebox) | Replaced by Scala 3 quotes/inline during feature-area restoration; the legacy `commons-macros` module is gone. |

## 2. Deprecated on Scala 3

*Populated as features are restored and intentionally kept behind `@deprecated` to ease downstream migration.*

| Symbol | Since | Replacement |
|--------|-------|-------------|
| *(empty in Phase 1)* | | |

## 3. Source-compat breaks

*Every downstream import/call site that changes vs the last Scala 2.13 release. Populated per restoration PR.*

### build / scalac options

- `-P:scalajs:mapSourceURI:...` → `-scalajs-mapSourceURI:...` — Scala.js 1.x on Scala 3 uses the native flag (no `-P:scalajs:` compiler-plugin prefix). Downstream consumers who copied our `jsCommonSettings` must update.
- `crossScalaVersions` removed — the build is single-axis Scala 3.8.2.
- Java CI matrix is Temurin 17 / 21 / 25 (matches upstream/scala-3 baseline).

### core

- Type parameter wildcards `K[_]` / `D[_]` / `C[_]` narrowed to `K[Any]` / `D[Any]` / `C[Any]` at a few derivation seams (`TypedMap`, `SelfInstance`, `HasGenCodec.wildcardCodec`). Scala 3 forbids HKT wildcard application; downstream callers using the explicit signatures may need to cast.
- `SelfInstance` HKT wildcard parameter narrowed to `C[Any]` (see `core/.../misc/SelfInstance.scala`).
- `Timestamp` no longer extends `Comparable[Timestamp]` — Scala 3 forbids `AnyVal` inheriting Object-derived traits. Use the explicit comparator.
- `SealedEnumCompanion.values` is `lazy val` instead of `val` (initialization order under Scala 3 sealed-children enumeration).
- `Tag` companion has an explicit `unapply` (Scala 3 case-class extractor inference changed).
- `SealedUtils.instancesFor` return type widened to `TC[T]`.
- `GenCodec.bseqCodec` / `iseqCodec` use the explicit `using` keyword (Scala 2 second-implicit-arg-list syntax no longer compiles).
- `enum` was renamed to `e` at one call site in `GenKeyCodec` (`enum` is reserved in Scala 3).
- `@targetName` annotation added to `CloseableIterator` overloaded methods.

### mongo

- `BsonRef.Creator.ref`, `DataTypeDsl.{ref, as, is, isNot}`, `TypedMongoUtils.optionalizeFirstArg` are stubbed with `???` pending Scala 3 macro reimplementation — callers still type-check but throw `NotImplementedError` at runtime.
- `E#IDType` type projections widened to `Any` in `MongoEntityCompanion`, `TypedMongoCollection`, `MongoEntityMeta` (Scala 3 forbids type projections on non-concrete prefixes). Public-API signature change.
- `BsonValueOutput.write` / `BsonValueInput.read` call sites require explicit `using` keyword.
- `MongoPolyDataCompanion` / `TypedMapFormat` / `TypedMapRefOps` widened from `K[_]` / `D[_]` to `K[Any]` / `D[Any]`.

### hocon

- `SealedEnumCompanion.values` override now `lazy val` (see core notes).

## 4. Binary-compat breaks

*Empty in Phase 1 — no Scala 3 baseline released yet. MiMa activation deferred to the first `_3` release tag.*

## 5. Disabled tests / modules

### Modules dropped from the `commons-jvm` aggregate

| Module | Reason | Restore effort |
|--------|--------|----------------|
| `commons-macros` | Deleted outright — pure Scala 2 macro infrastructure with no Scala 3 analogue worth porting. | n/a (will-not-migrate) |
| `analyzer` | Scala 2 compiler plugin. | L — dedicated phase |
| `jetty` | ee10 servlet wrapper. | M — dedicated phase |
| `spring` | spring-context wiring. | S — dedicated phase |
| `comprof` | `scalac-profiling` is Scala 2 only. | TBD |

### Test sources commented per-file

38 test classes commented across 38 files (whole-file `/* ... */` wraps) — every wrapped file had ALL classes broken with no surviving partners. Six broken-test categories:

- **TestMacros gone** — `core/src/test/.../macros/*.scala` (5 files): `ApplyUnapplyTest`, `JavaClassNameTest`, `KnownSubtypesTest`, `TreeForTypeTest`, `TypeStringTest`, plus `core/jvm/.../macros/TypeClassDerivationTest`.
- **`Components.???` stub** — `core/jvm/.../di/*.scala` (4 files): `ComponentComposition`, `ComponentsExample`, `ComponentsTest`, `MyApp`.
- **`GenCodec.materialize` stub** — `core/src/test/.../serialization/*.scala` and `core/jvm/.../serialization/*.scala` (≈14 files): `CodecTestData`, `GenCodecRoundtripTest`, `GenRefTest`, `IgnoreTransientDefaultMarkerTest`, `NotUsedTransientDefault`, `SimpleGenCodecTest`, `StreamGenCodecTest`, `StreamInputOutputTest`, `JCodecTestBase`, `JGenCodecTest`, `JsonGenCodecRoundtripTest`, `JsonStringInputOutputTest`, plus `CborInputOutputTest`, `HFloatTest`.
- **`MongoEntityCompanion` stub** — `mongo/jvm/.../*.scala` (9 files): `BsonInputOutputTest`, `MongoFilterTest`, `MongoIndexTest`, `MongoOrderTest`, `MongoProjectionTest`, `MongoRefTest`, `MongoUpdateTest`, `TypedMongoCollectionTest`, `testEntities`.
- **Hocon derivation** — `hocon/.../*.scala` (2 files): `HoconInputTest`, `HoconGenCodecRoundtripTest`.
- **Misc derivation** — `ImplicitNotFoundTest`, `MacroInstancesTest`, `CompilationErrorAssertions`.

Full per-file list with locations is in the Backlog table below (filter rows where Location contains `/src/test/`).

## Backlog

*Auto-derived from `git grep -nE 'TODO\[scala3-port\]'` on this PR's tip. Total tags: 155.*

| Location | Description | Effort |
|----------|-------------|--------|
| `core/jvm/src/test/scala/com/avsystem/commons/di/ComponentComposition.scala:4` | ComponentComposition — depends on Components.???-stubbed APIs | M |
| `core/jvm/src/test/scala/com/avsystem/commons/di/ComponentsExample.scala:4` | ComponentsExample — depends on Components.???-stubbed APIs | M |
| `core/jvm/src/test/scala/com/avsystem/commons/di/ComponentsTest.scala:4` | ComponentsTest — depends on Components.???-stubbed APIs | M |
| `core/jvm/src/test/scala/com/avsystem/commons/di/MyApp.scala:4` | MyApp — depends on Components.???-stubbed APIs | M |
| `core/jvm/src/test/scala/com/avsystem/commons/macros/TypeClassDerivationTest.scala:4` | TypeClassDerivationTest — depends on TestMacros / scala-2 `def ... = macro ...` | M |
| `core/jvm/src/test/scala/com/avsystem/commons/serialization/JCodecTestBase.scala:4` | JCodecTestBase — depends on commented serialization test data / stubbed materialize | M |
| `core/jvm/src/test/scala/com/avsystem/commons/serialization/JGenCodecTest.scala:4` | JGenCodecTest — depends on commented serialization test data / stubbed materialize | M |
| `core/src/main/scala/com/avsystem/commons/SharedExtensions.scala:129` | showAst (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/SharedExtensions.scala:131` | showRawAst (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/SharedExtensions.scala:133` | showSymbol (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/SharedExtensions.scala:135` | showSymbolFullName (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/SharedExtensions.scala:137` | showType (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/SharedExtensions.scala:139` | showRawType (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/SharedExtensions.scala:141` | showTypeSymbol (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/SharedExtensions.scala:143` | showTypeSymbolFullName (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/SharedExtensions.scala:145` | sourceCode (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/SharedExtensions.scala:147` | withSourceCode (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/annotation/AnnotationAggregate.scala:52` | reifyAggregated (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/annotation/positioned.scala:12` | here (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/di/Components.scala:18` | component (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/di/Components.scala:21` | asyncComponent (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/di/Components.scala:25` | singleton (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/di/Components.scala:28` | asyncSingleton (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/di/Components.scala:40` | reifyAllSingletons (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/di/Components.scala:49` | autoComponent (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/di/Components.scala:52` | optEmptyComponent (depends on stubbed singleton macro) | S |
| `core/src/main/scala/com/avsystem/commons/di/Components.scala:55` | noneComponent (depends on stubbed singleton macro) | S |
| `core/src/main/scala/com/avsystem/commons/di/Components.scala:58` | sequenceOpt (depends on stubbed component macro) | S |
| `core/src/main/scala/com/avsystem/commons/di/Components.scala:61` | sequenceOption (depends on stubbed component macro) | S |
| `core/src/main/scala/com/avsystem/commons/meta/AdtMetadataCompanion.scala:15` | materialize (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/meta/AdtMetadataCompanion.scala:18` | fromApplyUnapplyProvider (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/meta/AdtMetadataCompanion.scala:33` | materialize (bounded) (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/meta/AdtMetadataCompanion.scala:36` | fromApplyUnapplyProvider (bounded) (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/meta/MacroInstances.scala:47` | materialize (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala:27` | lazyMetadata (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/meta/MetadataCompanion.scala:58` | lazyMetadata (bounded) (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/meta/metaAnnotations.scala:193` | value (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/misc/AnnotationOf.scala:114` | SelfAnnotations.materialize (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/misc/AnnotationOf.scala:12` | AnnotationOf.materialize (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/misc/AnnotationOf.scala:22` | OptAnnotationOf.materialize (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/misc/AnnotationOf.scala:31` | AnnotationsOf.materialize (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/misc/AnnotationOf.scala:46` | HasAnnotation.materialize (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/misc/AnnotationOf.scala:69` | SelfAnnotation.materialize (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/misc/AnnotationOf.scala:92` | SelfOptAnnotation.materialize (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/misc/ApplierUnapplier.scala:13` | Applier.materialize (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/misc/ApplierUnapplier.scala:25` | Unapplier.materialize (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/misc/ApplierUnapplier.scala:37` | ApplierUnapplier.materialize (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/misc/Bidirectional.scala:6` | apply (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/misc/Delegation.scala:11` | materializeDelegation (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/misc/Delegation.scala:21` | CurriedDelegation.apply (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/misc/Implicits.scala:5` | infer (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/misc/Implicits.scala:7` | infer(clue) (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/misc/Implicits.scala:9` | inferNonMacro (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/misc/Sam.scala:9` | Sam.apply (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/misc/SamCompanion.scala:11` | SamCompanion.apply (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/misc/SamCompanion.scala:19` | isValidSam (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/misc/SealedUtils.scala:12` | instancesFor (Scala 2 macro def; return type widened to TC[T]) | L |
| `core/src/main/scala/com/avsystem/commons/misc/SealedUtils.scala:52` | caseObjects (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/misc/SealedUtils.scala:8` | caseObjectsFor (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/misc/SelfInstance.scala:4` | C[_] existential narrowed to C[Any] (Scala 3 forbids HKT wildcard application) | S |
| `core/src/main/scala/com/avsystem/commons/misc/SelfInstance.scala:7` | SelfInstance.materialize (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/misc/SimpleClassName.scala:8` | SimpleClassName.materialize (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/misc/SourceInfo.scala:28` | SourceInfo.here (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/misc/Timestamp.scala:13` | Comparable[Timestamp] (Scala 3 forbids AnyVal inheriting Object-derived traits) | S |
| `core/src/main/scala/com/avsystem/commons/misc/TypeString.scala:31` | TypeString.materialize (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/misc/TypeString.scala:90` | JavaClassName.materialize (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/misc/ValueEnum.scala:125` | ValueEnumCompanion.valName (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/rpc/AsRawReal.scala:100` | AsRawReal.materialize (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/rpc/AsRawReal.scala:113` | RpcMetadata.materialize (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/rpc/AsRawReal.scala:116` | RpcMetadata.materializeForApi (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/rpc/AsRawReal.scala:119` | RpcMetadata.auto (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/rpc/AsRawReal.scala:26` | AsRaw.materialize (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/rpc/AsRawReal.scala:29` | AsRaw.materializeForApi (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/rpc/AsRawReal.scala:62` | AsReal.materialize (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/rpc/RPCFramework.scala:127` | RPCCompanion.asRealRPC (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/rpc/RPCFramework.scala:129` | RPCCompanion.asRawRPC (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/rpc/RPCFramework.scala:131` | RPCCompanion.metadata (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/rpc/RPCFramework.scala:39` | materializeAsRaw (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/rpc/RPCFramework.scala:47` | materializeAsReal (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/rpc/RPCFramework.scala:55` | materializeAsRawReal (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/rpc/RPCFramework.scala:72` | materializeMetadata (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/rpc/RPCFramework.scala:99` | materializeFullInfo (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/rpc/RawRpcCompanion.scala:18` | RawRpcCompanion.materializeAsRaw (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/rpc/RawRpcCompanion.scala:20` | RawRpcCompanion.materializeAsReal (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/rpc/RawRpcCompanion.scala:22` | RawRpcCompanion.materializeAsRawReal (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/rpc/RawRpcCompanion.scala:24` | RawRpcCompanion.materializeApiAsRaw (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/rpc/RpcMetadataCompanion.scala:16` | RpcMetadataCompanion.materialize (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/rpc/RpcMetadataCompanion.scala:24` | ApiMetadataCompanion.materialize (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/rpc/RpcUtils.scala:56` | compilationError (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/serialization/GenCodec.scala:147` | GenCodec.forSealedEnum (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/serialization/GenCodec.scala:44` | GenCodec.materialize (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/serialization/GenCodec.scala:47` | GenCodec.fromApplyUnapplyProvider (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/serialization/GenCodec.scala:50` | GenCodec.applyUnapplyCodec (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/serialization/GenCodec.scala:53` | GenCodec.fromJavaBuilder (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/serialization/GenCodec.scala:610` | GenCodec.materializeRecursively (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/serialization/GenCodec.scala:613` | GenCodec.materializeImplicitly (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/serialization/GenKeyCodec.scala:28` | GenKeyCodec.forSealedEnum (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/serialization/GenKeyCodec.scala:31` | GenKeyCodec.forTransparentWrapper (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/serialization/GenObjectCodec.scala:37` | GenObjectCodec.materialize (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/serialization/GenObjectCodec.scala:40` | GenObjectCodec.fromApplyUnapplyProvider (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/serialization/GenRef.scala:27` | RawRef.Creator.ref (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/serialization/GenRef.scala:53` | GenRef.Creator.ref (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/serialization/GenRef.scala:58` | GenRef.Implicits.fun2GenRef (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/serialization/HasGenCodec.scala:143` | C[_] existential narrowed to C[Any] (Scala 3 forbids HKT wildcard application) | S |
| `core/src/main/scala/com/avsystem/commons/serialization/TupleGenCodecs.scala:5` | mkTupleCodec (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/serialization/macroCodecs.scala:107` | ApplyUnapplyCodec.materialize (Scala 2 macro def) | L |
| `core/src/main/scala/com/avsystem/commons/serialization/whenAbsent.scala:29` | whenAbsent.value (Scala 2 macro def) | L |
| `core/src/test/scala/com/avsystem/commons/macros/ApplyUnapplyTest.scala:4` | ApplyUnapplyTest — depends on TestMacros / scala-2 `def ... = macro ...` | L |
| `core/src/test/scala/com/avsystem/commons/macros/JavaClassNameTest.scala:4` | JavaClassNameTest — depends on TestMacros / scala-2 `def ... = macro ...` | L |
| `core/src/test/scala/com/avsystem/commons/macros/KnownSubtypesTest.scala:4` | KnownSubtypesTest — depends on TestMacros / scala-2 `def ... = macro ...` | L |
| `core/src/test/scala/com/avsystem/commons/macros/TreeForTypeTest.scala:4` | TreeForTypeTest — depends on TestMacros / scala-2 `def ... = macro ...` | L |
| `core/src/test/scala/com/avsystem/commons/macros/TypeStringTest.scala:4` | TypeStringTest — depends on TestMacros / scala-2 `def ... = macro ...` | L |
| `core/src/test/scala/com/avsystem/commons/misc/ImplicitNotFoundTest.scala:4` | ImplicitNotFoundTest — depends on stubbed materialize/derivation APIs | M |
| `core/src/test/scala/com/avsystem/commons/misc/MacroInstancesTest.scala:4` | MacroInstancesTest — depends on stubbed materialize/derivation APIs | M |
| `core/src/test/scala/com/avsystem/commons/serialization/CodecTestData.scala:4` | CodecTestData — depends on stubbed materialize/derivation APIs | M |
| `core/src/test/scala/com/avsystem/commons/serialization/GenCodecRoundtripTest.scala:4` | GenCodecRoundtripTest — depends on stubbed materialize/derivation APIs | M |
| `core/src/test/scala/com/avsystem/commons/serialization/GenRefTest.scala:4` | GenRefTest — depends on stubbed materialize/derivation APIs | M |
| `core/src/test/scala/com/avsystem/commons/serialization/IgnoreTransientDefaultMarkerTest.scala:4` | IgnoreTransientDefaultMarkerTest — depends on commented serialization test data / stubbed materialize | M |
| `core/src/test/scala/com/avsystem/commons/serialization/NotUsedTransientDefault.scala:3` | NotUsedTransientDefault — depends on stubbed materialize/derivation APIs | M |
| `core/src/test/scala/com/avsystem/commons/serialization/SimpleGenCodecTest.scala:4` | SimpleGenCodecTest — depends on stubbed materialize/derivation APIs | M |
| `core/src/test/scala/com/avsystem/commons/serialization/StreamGenCodecTest.scala:4` | StreamGenCodecTest — depends on commented serialization test data / stubbed materialize | M |
| `core/src/test/scala/com/avsystem/commons/serialization/StreamInputOutputTest.scala:4` | StreamInputOutputTest — depends on stubbed materialize/derivation APIs | M |
| `core/src/test/scala/com/avsystem/commons/serialization/cbor/CborInputOutputTest.scala:4` | CborInputOutputTest — depends on stubbed materialize/derivation APIs | M |
| `core/src/test/scala/com/avsystem/commons/serialization/cbor/HFloatTest.scala:4` | HFloatTest — depends on stubbed materialize/derivation APIs | M |
| `core/src/test/scala/com/avsystem/commons/serialization/json/JsonGenCodecRoundtripTest.scala:4` | JsonGenCodecRoundtripTest — depends on commented serialization test data / stubbed materialize | M |
| `core/src/test/scala/com/avsystem/commons/serialization/json/JsonStringInputOutputTest.scala:4` | JsonStringInputOutputTest — depends on commented serialization test data / stubbed materialize | M |
| `core/src/test/scala/com/avsystem/commons/testutil/CompilationErrorAssertions.scala:4` | CompilationErrorAssertions — depends on stubbed materialize/derivation APIs | M |
| `hocon/src/test/scala/com/avsystem/commons/hocon/HoconGenCodecRoundtripTest.scala:4` | HoconGenCodecRoundtripTest — depends on stubbed materialize / mongo entity companions | M |
| `hocon/src/test/scala/com/avsystem/commons/hocon/HoconInputTest.scala:4` | HoconInputTest — depends on stubbed materialize / mongo entity companions | M |
| `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonRef.scala:28` | was Scala 2 macro `BsonRefMacros.bsonRef`; stub keeps callers compiling | L |
| `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/DataTypeDsl.scala:131` | was Scala 2 macro `MongoMacros.refImpl`; stub keeps callers compiling | L |
| `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/DataTypeDsl.scala:164` | was Scala 2 macro `MongoMacros.asSubtype`; stub keeps callers compiling | M |
| `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/DataTypeDsl.scala:193` | was Scala 2 macro `MongoMacros.isSubtype`; stub keeps callers compiling | M |
| `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/DataTypeDsl.scala:199` | was Scala 2 macro `MongoMacros.isNotSubtype`; stub keeps callers compiling | M |
| `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoEntityCompanion.scala:57` | `E#IDType` type projection forbidden on abstract types; widen to Any to keep signatures | M |
| `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoFormat.scala:107` | K[_] → K[Any] workaround for Scala 3 wildcard-as-type-arg restriction | S |
| `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoFormat.scala:364` | `E#IDType` type projection forbidden on abstract types; widen to Any to keep signatures | M |
| `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoFormat.scala:77` | K[_] → K[Any] workaround for Scala 3 wildcard-as-type-arg restriction | S |
| `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoPolyDataCompanion.scala:33` | D[_] → D[Any] workaround for Scala 3 wildcard-as-type-arg restriction | S |
| `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoRef.scala:272` | cast follows K[_] → K[Any] workaround in TypedMapFormat | S |
| `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/TypedMongoCollection.scala:36` | `E#IDType` type projection forbidden on abstract types; widen to Any to keep signatures | M |
| `mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/TypedMongoUtils.scala:23` | was Scala 2 macro `MiscMacros.optionalizeFirstArg`; stub keeps callers compiling | M |
| `mongo/jvm/src/test/scala/com/avsystem/commons/mongo/BsonInputOutputTest.scala:4` | BsonInputOutputTest — depends on stubbed materialize / mongo entity companions | M |
| `mongo/jvm/src/test/scala/com/avsystem/commons/mongo/typed/MongoFilterTest.scala:4` | MongoFilterTest — depends on commented testEntities | M |
| `mongo/jvm/src/test/scala/com/avsystem/commons/mongo/typed/MongoIndexTest.scala:4` | MongoIndexTest — depends on commented testEntities | M |
| `mongo/jvm/src/test/scala/com/avsystem/commons/mongo/typed/MongoOrderTest.scala:4` | MongoOrderTest — depends on commented testEntities | M |
| `mongo/jvm/src/test/scala/com/avsystem/commons/mongo/typed/MongoProjectionTest.scala:4` | MongoProjectionTest — depends on commented testEntities | M |
| `mongo/jvm/src/test/scala/com/avsystem/commons/mongo/typed/MongoRefTest.scala:4` | MongoRefTest — depends on commented testEntities | M |
| `mongo/jvm/src/test/scala/com/avsystem/commons/mongo/typed/MongoUpdateTest.scala:4` | MongoUpdateTest — depends on stubbed materialize / mongo entity companions | M |
| `mongo/jvm/src/test/scala/com/avsystem/commons/mongo/typed/TypedMongoCollectionTest.scala:4` | TypedMongoCollectionTest — depends on commented testEntities | M |
| `mongo/jvm/src/test/scala/com/avsystem/commons/mongo/typed/testEntities.scala:4` | testEntities — depends on stubbed materialize / mongo entity companions | M |
| `project/Commons.scala:130` | enable -Werror after warnings clean |  |
| `project/Commons.scala:203` | Scala 2 compiler plugin; restore as Scala 3 plugin | L |
| `project/Commons.scala:206` | ee10 servlet wrapper | M |
| `project/Commons.scala:210` | spring-context wiring | S |
