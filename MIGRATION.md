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
| macros | cross | stub | n/a | n/a | Empty scala-3 dir; whitebox impls remain 2.13-only. |
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

Seeded from a `@deprecated` scan of fork `master` on `origin/master@bcc3bcbf` (`2026-05-31`). Re-runnable verbatim:

```bash
git grep -n '@deprecated' master -- '*.scala'
```

Lines tagged `[skip-port]` have a Scala standard library or language-feature replacement and are not ported during the Scala 3 migration. Lines tagged `[port]` reference internal replacements and must be addressed by the relevant module port. Messages are truncated to ~80 characters.

### core/

```
core/src/main/scala-2.13/com/avsystem/commons/SharedExtensions.scala:840 — orElse — "Scala 2.13 has native scala.math.Ordering.orElse implementation" [skip-port]
core/src/main/scala-2.13/com/avsystem/commons/SharedExtensions.scala:848 — orElseBy — "Scala 2.13 has native scala.math.Ordering.orElseBy implementation" [skip-port]
core/src/main/scala-2.13/com/avsystem/commons/misc/Sam.scala:4 — Sam — "Use native SAM conversion instead, e.g. `val r: Runnable = () => doStuff()` or …" [port]
core/src/main/scala-2.13/com/avsystem/commons/misc/SamCompanion.scala:6 — SamCompanion — "Use native SAM conversion instead, e.g. `val r: Runnable = () => doStuff()` or …" [port]
core/src/main/scala-2.13/com/avsystem/commons/misc/ValueOf.scala:10 — ValueOf — "Use scala.ValueOf[T] from the standard library - it is auto-materialized by the…" [skip-port]
core/src/main/scala-2.13/com/avsystem/commons/misc/ValueOf.scala:20 — apply — "Use scala.valueOf[T] from the standard library (available since Scala 2.13)" [skip-port]
core/src/main/scala-2.13/com/avsystem/commons/misc/ValueOf.scala:23 — fromScala — "Use scala.ValueOf[T] from the standard library (available since Scala 2.13)" [skip-port]
core/src/main/scala-2.13/com/avsystem/commons/rpc/AsRawReal.scala:17 — create — "use SAM syntax (lambda)" [skip-port]
core/src/main/scala-2.13/com/avsystem/commons/rpc/AsRawReal.scala:56 — create — "use SAM syntax (lambda)" [skip-port]
core/src/main/scala-2.13/com/avsystem/commons/serialization/GenCodec.scala:82 — applyUnapplyCodec — "Use GenCodec.materialize instead; ApplyUnapplyCodec is being removed." [port]
core/src/main/scala-2.13/com/avsystem/commons/serialization/HasGenCodec.scala:20 — HasApplyUnapplyCodec — "Use HasGenCodec instead; ApplyUnapplyCodec is being removed." [port]
core/src/main/scala-2.13/com/avsystem/commons/serialization/HasGenCodec.scala:48 — HasApplyUnapplyCodecWithDeps — "Use HasGenCodecWithDeps instead; ApplyUnapplyCodec is being removed." [port]
core/src/main/scala-2.13/com/avsystem/commons/serialization/cbor/CborKeyCodec.scala:23 — fromFieldLabels — "use CborKeyCodec instead" [port]
core/src/main/scala-2.13/com/avsystem/commons/serialization/cbor/CborKeyCodec.scala:44 — FieldLabels — "use CborKeyCodec instead" [port]
core/src/main/scala-2.13/com/avsystem/commons/serialization/cbor/CborKeyCodec.scala:50 — NoLabels — "use CborKeyCodec instead" [port]
core/src/main/scala-2.13/com/avsystem/commons/serialization/macroCodecs.scala:107 — materialize — "Use GenCodec.materialize instead; ApplyUnapplyCodec is being removed." [port]
core/src/main/scala-3/com/avsystem/commons/misc/Bidirectional.scala:7 — Bidirectional — "Bidirectional macro not ported to Scala 3 — write the reversed PartialFunction …" [port]
core/src/main/scala-3/com/avsystem/commons/misc/Bidirectional.scala:11 — Bidirectional — "Bidirectional macro not ported to Scala 3 — write the reversed PartialFunction …" [port]
core/src/main/scala-3/com/avsystem/commons/misc/TypeString.scala:93 — derivedImpl — "Use JavaClassName.derived instead" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:7 — BooleanBoxing — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:9 — ByteBoxing — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:11 — CharBoxing — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:13 — IntBoxing — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:15 — LongBoxing — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:17 — FloatBoxing — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:19 — DoubleBoxing — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:24 — nullableBoxing — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:29 — BooleanBoxing — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:31 — ByteUnboxing — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:33 — CharBoxing — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:35 — IntBoxing — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:37 — LongBoxing — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:39 — FloatBoxing — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:41 — DoubleBoxing — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:46 — nullableBoxing — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:51 — opt2Iterable — "Use given Conversion directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:56 — opt2Iterable — "Use given Conversion directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:61 — opt2Iterable — "Use given Conversion directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:66 — conversions — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:68 — ordering — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:73 — keyCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:75 — codec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:80 — NothingClassName — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:82 — NothingArrayClassName — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:84 — UnitClassName — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:86 — BooleanClassName — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:88 — ByteClassName — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:90 — ShortClassName — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:92 — IntClassName — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:94 — LongClassName — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:96 — FloatClassName — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:98 — DoubleClassName — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:100 — CharClassName — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:102 — AnyClassName — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:104 — AnyValClassName — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:106 — keyCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:108 — codec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:110 — arrayClassName — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:115 — keyCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:117 — codec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:122 — ordering — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/misc/compat.scala:127 — ordering — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodec.scala:59 — fromApplyUnapplyProvider — "Use GenCodec.derived[T] instead; ApplyUnapplyCodec is being phased out." [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodec.scala:63 — fromApplyUnapplyProvider — "Use GenCodec.transform with a named tuple instead" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:11 — NothingCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:13 — NullCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:15 — UnitCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:17 — VoidCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:19 — BooleanCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:21 — CharCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:23 — ByteCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:25 — ShortCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:27 — IntCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:29 — LongCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:31 — FloatCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:33 — DoubleCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:35 — BigIntCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:37 — BigDecimalCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:39 — JBooleanCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:41 — JCharacterCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:43 — JByteCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:45 — JShortCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:47 — JIntegerCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:49 — JLongCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:51 — JFloatCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:53 — JDoubleCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:55 — JBigIntegerCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:57 — JBigDecimalCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:59 — JDateCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:61 — StringCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:63 — SymbolCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:65 — ArrayByteCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:67 — UUIDCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:69 — TimestampCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:71 — BytesCodec — "Use given instance directly" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:74 — GenCodecCompat — "Use GenCodec.deriveRecursively instead" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCompat.scala:77 — GenCodecCompat — "Use GenCodec.derived[T] explicitly or rely on AllowDerivation/AllowRecursiveDer…" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCreates.scala:45 — nullSafe — "Use `create` (always null-safe) or `createNullable` for explicit T | Null" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCreates.scala:49 — nullable — "Use `createNullable` for explicit T | Null" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCreates.scala:53 — nonNull — "Use `create` instead" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCreates.scala:57 — nullableString — "Use `createString` instead" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCreates.scala:61 — nonNullString — "Use `createString` instead" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCreates.scala:65 — createSimple — "Use `createSimple` without `allowNull`" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCreates.scala:69 — nullableSimple — "Use `createSimple` instead" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCreates.scala:73 — nonNullSimple — "Use `createSimple` instead" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCreates.scala:77 — createList — "Use `createList` without `allowNull`" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCreates.scala:81 — nullableList — "Use `createList` instead" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCreates.scala:85 — nonNullList — "Use `createList` instead" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCreates.scala:89 — createObject — "Use `createObject` without `allowNull`" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCreates.scala:97 — nullableObject — "Use `createObject` instead" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecCreates.scala:101 — nonNullObject — "Use `createObject` instead" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecImpl.scala:21 — ApplyUnapplyCodec — "Use HasGenCodec / GenCodec.derived for case classes." [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecImpl.scala:93 — ApplyUnapplyCodec — "Use HasGenCodec / GenCodec.derived for case classes." [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecImpl.scala:105 — ProductCodec — "Use HasGenCodec / GenCodec.derived for case classes." [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecImpl.scala:501 — Deferred — "Use DeferredCodec instead" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/GenCodecImpl.scala:507 — Transformed — "Use TransformedCodec instead" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/HasGenCodec.scala:36 — HasApplyUnapplyCodec — "Use HasGenCodec instead; ApplyUnapplyCodec is being phased out." [port]
core/src/main/scala-3/com/avsystem/commons/serialization/HasGenCodec.scala:63 — HasApplyUnapplyCodecWithDeps — "Use HasGenCodecWithDeps instead; ApplyUnapplyCodec is being phased out." [port]
core/src/main/scala-3/com/avsystem/commons/serialization/HasGenCodec.scala:185 — AUCodec — "Use HasGenCodec / GenCodec.transform with a named tuple instead." [port]
core/src/main/scala-3/com/avsystem/commons/serialization/HasGenCodec.scala:196 — HasGenCodecFromAU — "Use HasGenCodec / GenCodec.transform with a named tuple instead." [port]
core/src/main/scala-3/com/avsystem/commons/serialization/cbor/CborKeyCodec.scala:23 — fromFieldLabels — "use CborKeyCodec instead" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/cbor/CborKeyCodec.scala:44 — FieldLabels — "use CborKeyCodec instead" [port]
core/src/main/scala-3/com/avsystem/commons/serialization/cbor/CborKeyCodec.scala:50 — NoLabels — "use CborKeyCodec instead" [port]
```

### mongo/

```
mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonGenCodecs.scala:77 — objectIdIdentityWrapping — "Use summon[TransparentWrapping[ObjectId, ObjectId]]" [port]
mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonGenCodecs.scala:79 — objectIdCodec — "Use summon[GenCodec[ObjectId]]" [port]
mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonGenCodecs.scala:81 — objectIdKeyCodec — "Use summon[GenKeyCodec[ObjectId]]" [port]
mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonGenCodecs.scala:83 — decimal128Codec — "Use summon[GenCodec[Decimal128]]" [port]
mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonGenCodecs.scala:85 — bsonValueCodec — "Use summon[GenCodec[BsonValue]]" [port]
mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonGenCodecs.scala:87 — bsonArrayCodec — "Use summon[GenCodec[BsonArray]]" [port]
mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonGenCodecs.scala:89 — bsonBinaryCodec — "Use summon[GenCodec[BsonBinary]]" [port]
mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonGenCodecs.scala:91 — bsonBooleanCodec — "Use summon[GenCodec[BsonBoolean]]" [port]
mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonGenCodecs.scala:93 — bsonDateTimeCodec — "Use summon[GenCodec[BsonDateTime]]" [port]
mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonGenCodecs.scala:95 — bsonDocumentCodec — "Use summon[GenCodec[BsonDocument]]" [port]
mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonGenCodecs.scala:97 — bsonDecimal128Codec — "Use summon[GenCodec[BsonDecimal128]]" [port]
mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonGenCodecs.scala:99 — bsonDoubleCodec — "Use summon[GenCodec[BsonDouble]]" [port]
mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonGenCodecs.scala:101 — bsonInt32Codec — "Use summon[GenCodec[BsonInt32]]" [port]
mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonGenCodecs.scala:103 — bsonInt64Codec — "Use summon[GenCodec[BsonInt64]]" [port]
mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonGenCodecs.scala:105 — bsonNullCodec — "Use summon[GenCodec[BsonNull]]" [port]
mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonGenCodecs.scala:107 — bsonObjectIdCodec — "Use summon[GenCodec[BsonObjectId]]" [port]
mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonGenCodecs.scala:109 — bsonStringCodec — "Use summon[GenCodec[BsonString]]" [port]
mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonRef.scala:50 — bsonRefIterableUpdating — "Use summon[Conversion[BsonRef[S, C[E]], BsonRefIterableUpdating[E, C]]] or rely…" [port]
mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonRef.scala:57 — bsonRefUpdating — "Use summon[Conversion[BsonRef[S, T], BsonRefUpdating[T]]] or rely on implicit c…" [port]
mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonRef.scala:62 — bsonRefSorting — "Use summon[Conversion[BsonRef[S, T], BsonRefSorting[T]]] or rely on implicit co…" [port]
mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonRef.scala:67 — bsonRefIterableFiltering — "Use summon[Conversion[BsonRef[S, C[E]], BsonRefIterableFiltering[E, C]]] or rel…" [port]
mongo/jvm/src/main/scala/com/avsystem/commons/mongo/BsonRef.scala:74 — bsonRefFiltering — "Use summon[Conversion[BsonRef[S, T], BsonRefFiltering[T]]] or rely on implicit …" [port]
mongo/jvm/src/main/scala/com/avsystem/commons/mongo/Filter.scala:21 — eq — "(message not captured)" [port]
mongo/jvm/src/main/scala/com/avsystem/commons/mongo/Filter.scala:24 — ne — "(message not captured)" [port]
mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoPolyDataCompanion.scala:10 — MongoPolyAdtInstances — "MongoPolyAdtInstances not ported to scala-3 (polymorphic instance methods unsup…" [port]
mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoPolyDataCompanion.scala:16 — AbstractMongoPolyDataCompanion — "AbstractMongoPolyDataCompanion not ported to scala-3" [port]
mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/MongoPolyDataCompanion.scala:19 — MongoPolyDataCompanion — "MongoPolyDataCompanion not ported to scala-3" [port]
mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/TypedMongoClient.scala:61 — listDatabases — "Use listTypedDatabases or listRawDatabases instead" [port]
mongo/jvm/src/main/scala/com/avsystem/commons/mongo/typed/TypedMongoDatabase.scala:50 — listDatabases — "Use listTypedCollections or listRawCollections instead" [port]
```
