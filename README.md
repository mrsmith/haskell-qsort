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
| 100     | 0.000004s (±31.5%)   | 25,000,000 | 0.000028s (±8.7%)   | 3,571,429      | 7x    |
| 1,000   | 0.000027s (±21.5%)   | 37,037,037 | 0.000387s (±0.6%)   | 2,583,979      | 14x   |
| 10,000  | 0.000382s (±1.0%)    | 26,178,010 | 0.005032s (±3.1%)   | 1,987,281      | 13x   |
| 100,000 | 0.004816s (±0.2%)    | 20,764,120 | 0.078278s (±0.8%)   | 1,277,498      | 16x   |

### Key Observations

- C implementation is 7-16x faster than Haskell
- Both implementations correctly sort all test cases
- C shows consistent ~20-37M elements/second throughput
- Haskell achieves ~1-4M elements/second across all sizes
- **Unified methodology**: Both implementations use identical statistical approaches

### Measurement Reliability

The **±CV%** shows timing stability with the unified methodology:
- **C**: Variable stability (±0.2-31.5%) - higher variance for small inputs is expected
- **Haskell**: Good stability (±0.6-8.7%) across all input sizes  
- **Lower CV% = more reliable** measurements

**Unified Statistical Method**: 
- **Both implementations**: 1 warmup run + 4 measurement runs using multiplier approach (data × run_number)
- **Timing**: Nanosecond precision using `clock_gettime` (C) and `getCPUTime` (Haskell)
- **Analysis**: Report median time with coefficient of variation (standard deviation / median × 100%)

The performance difference reflects the trade-off between C's imperative, memory-efficient approach and Haskell's functional, list-based implementation that creates intermediate data structures.