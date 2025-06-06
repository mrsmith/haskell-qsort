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

#### Random Data (Best Case)
| Algorithm | 100 elem/s | 1K elem/s | 10K elem/s | 100K elem/s | Notes |
|-----------|------------|-----------|------------|-------------|-------|
| **C Quicksort** | 50M | 48M | 26M | 21M | Our implementation |
| **C qsort()** | 50M | 30M | 18M | 15M | Standard library |
| **Haskell Quicksort** | 4M | 3M | 2M | 1M | Functional lists |
| **Haskell In-place** | 13M | 7M | 4M | 2M | Mutable arrays |
| **Haskell sort** | 9M | 6M | 3M | 2M | Data.List.sort |

#### Duplicate Data (Worst Case)
| Algorithm | 100 elem/s | 1K elem/s | 10K elem/s | 100K elem/s | Performance |
|-----------|------------|-----------|------------|-------------|-------------|
| **C Quicksort** | 100M | 24M | 3M | 317K | **O(n²) degradation** |
| **C qsort()** | 100M | 250M | 125M | 114M | **Excellent** (introsort) |
| **Haskell Quicksort** | 3M | 457K | 54K | ~10K* | **O(n²) degradation** |
| **Haskell In-place** | 8M | 1.3M | 146K | 15K | **O(n²) degradation** |
| **Haskell sort** | ~40M* | ~20M* | ~10M* | ~5M* | **Good** (mergesort) |

*Estimated - some tests timed out due to O(n²) behavior

### Key Algorithmic Insights

**Custom Quicksort vs Standard Libraries:**
- **Random data**: Custom implementations competitive with standard libraries
- **Duplicate data**: Standard libraries **dramatically superior** (100-1000x faster)
- **C qsort()**: Uses introsort (quicksort + heapsort fallback) → O(n log n) guaranteed
- **Haskell sort**: Uses mergesort → O(n log n) guaranteed, often faster than quicksort

**Language Performance:**
- **C vs Haskell**: 5-25x performance advantage for C across all algorithms
- **Memory matters in Haskell**: In-place arrays 2x faster than functional lists
- **Fair comparison**: In-place Haskell vs C shows ~10x difference (algorithmic equivalence)
- **Compiler optimization**: Both GCC and GHC produce highly optimized code

### Statistical Methodology

- **Measurements**: 1 warmup + 5 measurement runs (median reported)
- **Iterations**: 1000x for n<1000, 100x for n<10000, 1x for n≥10000
- **Timing**: Nanosecond precision (`clock_gettime`, `getCPUTime`)
- **Variance**: Coefficient of variation (CV) typically ±0.1-7% (excellent stability)
- **Input Types**: Random, sorted, reverse, duplicate-heavy data

### Conclusions

**Algorithm Choice Matters:**
- **For production use**: Standard library sorts (qsort, Data.List.sort) are superior
- **Worst-case protection**: Introsort and mergesort prevent O(n²) degradation
- **Educational value**: Our quicksort demonstrates classic algorithmic behavior

**Language Performance:**
- **C dominance**: 5-25x faster than Haskell due to memory efficiency and compilation
- **Haskell competitiveness**: Data.List.sort often outperforms our Haskell quicksort
- **Implementation quality**: Both languages can achieve excellent performance with proper algorithms

**Key Takeaway**: This benchmark demonstrates why production systems use hybrid algorithms (introsort, timsort) rather than pure quicksort - **worst-case protection is essential** for reliable performance.