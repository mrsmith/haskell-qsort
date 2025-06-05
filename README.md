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

# Run individual benchmarks
make bench-c
make bench-haskell

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
| 100     | 0.000004   | 25,000,000 | 0.000832         | 120,192        | 208x  |
| 1,000   | 0.000041   | 24,390,244 | 0.001194         | 837,521        | 29x   |
| 10,000  | 0.000463   | 21,598,272 | 0.009057         | 1,104,118      | 20x   |
| 100,000 | 0.004828   | 20,712,510 | 0.093775         | 1,066,382      | 19x   |

### Key Observations

- C implementation is 19-208x faster than Haskell
- Performance gap narrows as dataset size increases
- Both implementations correctly sort all test cases
- C shows consistent ~20M elements/second throughput
- Haskell achieves ~1M elements/second for larger datasets

The performance difference reflects the trade-off between C's imperative, memory-efficient approach and Haskell's functional, list-based implementation that creates intermediate data structures.