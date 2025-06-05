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

| Size    | C Time (s) | C Elem/s   | Haskell Time (s) | Haskell Elem/s | Ratio |
|---------|------------|------------|------------------|----------------|-------|
| 100     | 0.000011   | 9,090,909  | 0.000559         | 178,891        | 51x   |
| 1,000   | 0.000038   | 26,315,789 | 0.000983         | 1,017,294      | 26x   |
| 10,000  | 0.000413   | 24,213,075 | 0.009015         | 1,109,262      | 22x   |
| 100,000 | 0.004857   | 20,588,841 | 0.093457         | 1,070,011      | 19x   |

### Key Observations

- C implementation is 19-51x faster than Haskell
- Performance gap narrows as dataset size increases
- Both implementations correctly sort all test cases
- C shows consistent ~20M elements/second throughput
- Haskell achieves ~1M elements/second for larger datasets
- Optimized codebase: 133 total lines (was 197), -32% reduction

The performance difference reflects the trade-off between C's imperative, memory-efficient approach and Haskell's functional, list-based implementation that creates intermediate data structures.

## Code Optimizations

The project has been optimized for conciseness while maintaining full functionality:

- **benchmark.c**: Reduced from 72 to 61 lines (-15%)
- **Benchmark.hs**: Reduced from 20 to 15 lines (-25%)  
- **run_benchmark.sh**: Reduced from 26 to 16 lines (-38%)
- **Makefile**: Reduced from 35 to 28 lines (-20%)

Optimizations include removing redundant code, using idiomatic patterns, combining operations, and eliminating unnecessary complexity.