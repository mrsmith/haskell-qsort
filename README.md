# Quicksort Performance Comparison: C vs Haskell

This project compares the performance of quicksort implementations in C and Haskell using identical test data and measurement methodologies.

## Overview

- **C Implementation**: In-place quicksort using Lomuto partition scheme
- **Haskell Implementation**: Functional quicksort using filter-based approach
- **Benchmarking**: CPU time measurement excluding I/O operations
- **Verification**: Automatic sorting correctness validation

## Prerequisites

- GCC compiler with optimization support
- GHC (Glasgow Haskell Compiler)
- Python 3 (for test data generation)
- Make utility
- Unix-like environment (macOS/Linux)

## Usage

```bash
# Build everything and run both benchmarks
make run

# Build only
make build

# Generate test data
make test-data

# Clean up
make clean
```

### Manual Usage

```bash
# Run specific benchmark
./run_benchmark.sh c_benchmark
./run_benchmark.sh haskell_benchmark
```

## Results

### Test Machine
- **Platform**: macOS (Darwin 24.4.0)
- **Architecture**: ARM64 (Apple Silicon)
- **Compiler Flags**: 
  - C: `-O3 -march=native -mtune=native -flto`
  - Haskell: `-O2 -funbox-strict-fields -fexcess-precision -optc-O3`

### Performance Results

| Size    | C Time (±CV%)         | C Elem/s   | Haskell Time (±CV%)  | Haskell Elem/s | Ratio |
|---------|----------------------|------------|----------------------|----------------|-------|
| 100     | 0.000001s (±0.3%)    | 100,000,000| 0.000023s (±1.4%)   | 4,347,826      | 23x   |
| 1,000   | 0.000014s (±0.6%)    | 71,428,571 | 0.000384s (±0.5%)   | 2,604,167      | 27x   |
| 10,000  | 0.000379s (±1.1%)    | 26,385,224 | 0.006100s (±7.2%)   | 1,639,344      | 16x   |
| 100,000 | 0.004888s (±1.3%)    | 20,458,265 | 0.086427s (±2.8%)   | 1,157,046      | 18x   |

### Key Observations

- C implementation is 16-27x faster than Haskell
- Both implementations correctly sort all test cases
- C shows consistent ~20-100M elements/second throughput
- Haskell achieves ~1-4M elements/second across all sizes
- **Unified methodology**: Both implementations use identical statistical approaches with iterations

### Measurement Reliability

The **±CV%** shows timing stability with the unified methodology:
- **C**: Excellent stability (±0.3-1.3%) across all input sizes
- **Haskell**: Good stability (±0.5-7.2%) across all input sizes  
- **Lower CV% = more reliable** measurements

**Unified Statistical Method**: 
- **Both implementations**: 1 warmup run + 4 measurement runs using multiplier approach
- **Iterations**: 1000x for n<1000, 100x for n<10000, 1x for n≥10000 (identical in both)
- **Timing**: Nanosecond precision using `clock_gettime` (C) and `getCPUTime` (Haskell)
- **Analysis**: Report median time with coefficient of variation (standard deviation / median × 100%)

The performance difference reflects the trade-off between C's imperative, memory-efficient approach and Haskell's functional, list-based implementation that creates intermediate data structures.