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
| 100     | 0.000002s (±6.2%)    | 50,000,000 | 0.000025s (±3.3%)   | 4,000,000      | 12x   |
| 1,000   | 0.000016s (±1.9%)    | 62,500,000 | 0.000386s (±0.9%)   | 2,590,674      | 24x   |
| 10,000  | 0.000376s (±1.1%)    | 26,595,745 | 0.005028s (±4.1%)   | 1,988,862      | 13x   |
| 100,000 | 0.004839s (±2.2%)    | 20,665,427 | 0.078737s (±1.1%)   | 1,270,051      | 16x   |

### Key Observations

- C implementation is 12-24x faster than Haskell
- Both implementations correctly sort all test cases
- C shows consistent ~20-60M elements/second throughput
- Haskell achieves ~1-4M elements/second across all sizes
- **Excellent statistical stability**: Both implementations now show ±1-6% variance

### Measurement Reliability

The **±CV%** shows timing stability - both implementations now achieve professional-grade reliability:
- **C**: Excellent stability (±1.1-6.2%) across all input sizes
- **Haskell**: Excellent stability (±0.9-4.1%) across all input sizes
- **Lower CV% = more reliable** measurements

**Advanced Statistical Method**: 
- **C**: Adaptive iterations (1000x for small inputs), nanosecond timer (`clock_gettime`), 3 warmup runs for small inputs
- **Haskell**: 5-run median with multiplier approach, 1 warmup run
- Report median time with coefficient of variation (standard deviation / median × 100%)

The performance difference reflects the trade-off between C's imperative, memory-efficient approach and Haskell's functional, list-based implementation that creates intermediate data structures.