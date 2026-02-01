# Benchmarks

This repository contains two benchmark modules to compare performance between Scala 2 and Scala 3 implementations:

## Benchmark Modules

### `benchmark` (Scala 3)
- **Scala Version**: 3.8.1
- **Dependencies**: Depends on the local `core` module (new implementation)
- **Purpose**: Benchmarks the current Scala 3 implementation

### `benchmarks2` (Scala 2)
- **Scala Version**: 2.13.18  
- **Dependencies**: Depends on the published `commons-core` library version 2.2.4
- **Purpose**: Benchmarks the previously published Scala 2 implementation for comparison

## Shared Benchmark Code

Both modules use the same benchmark code to ensure fair comparisons:
- Benchmark sources are located in `benchmarks2/src/main/scala/`
- The `benchmark` module uses its sources from `benchmark/jvm/src/main/scala/` and `benchmark/src/main/scala/`
- Scala 2-specific compatibility code is in `benchmark/jvm/src/main/scala-2.13/`

## Removed Benchmarks

MongoDB and Redis-related benchmarks have been removed as they are no longer relevant for the core library comparison.

## Running Benchmarks Locally

### Run Scala 3 benchmarks:
```bash
sbt "project benchmark" "Jmh/run"
```

### Run Scala 2 benchmarks:
```bash
sbt "project benchmarks2" "Jmh/run"
```

### Run specific benchmark:
```bash
sbt "project benchmark" "Jmh/run -i 5 -wi 3 -f 1 .*someWriting"
```

## GitHub Actions Workflow

The `.github/workflows/benchmark.yml` workflow automatically:
1. Runs benchmarks on both Scala 2 and Scala 3 implementations on every PR
2. Generates JSON results for both versions
3. Compares the results and posts a comment to the PR with:
   - Performance metrics (ops/second)
   - Percentage improvements/regressions
   - Visual indicators (🟢 for improvements, 🔴 for regressions)

## Benchmark Output

Benchmark results are saved in JSON format:
- `benchmark-scala3-results.json` - Scala 3 implementation results
- `benchmark-scala2-results.json` - Scala 2 published version results

These files are uploaded as GitHub Actions artifacts for historical tracking.
