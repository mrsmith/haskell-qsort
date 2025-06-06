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
| 100     | 0.000002s (±74.2%)   | 50,000,000 | 0.000024s (±246.0%) | 4,166,667      | 12x   |
| 1,000   | 0.000022s (±39.2%)   | 45,454,545 | 0.000380s (±108.4%) | 2,631,579      | 17x   |
| 10,000  | 0.000387s (±3.5%)    | 25,839,793 | 0.005174s (±86.0%)  | 1,932,741      | 13x   |
| 100,000 | 0.004888s (±1.2%)    | 20,458,265 | 0.079379s (±57.5%)  | 1,259,779      | 16x   |

### Key Observations

- C implementation is 12-17x faster than Haskell
- Both implementations correctly sort all test cases
- C shows consistent ~20-50M elements/second throughput
- Haskell achieves ~1-4M elements/second across all sizes
- **Statistical stability**: Both use 5-run median with coefficient of variation (CV%)

### Measurement Reliability

The **±CV%** shows timing stability:
- **C**: Very stable on larger inputs (±1.2-3.5%), more variable on small inputs
- **Haskell**: More variable overall (±57-246%), typical for functional implementations
- **Lower CV% = more reliable** measurements

**Statistical Method**: 5 runs per size, report median time with coefficient of variation (standard deviation / median × 100%)

The performance difference reflects the trade-off between C's imperative, memory-efficient approach and Haskell's functional, list-based implementation that creates intermediate data structures.