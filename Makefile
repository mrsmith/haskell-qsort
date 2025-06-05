# Compiler settings
CC = gcc
CFLAGS = -O3 -march=native -mtune=native -flto
GHC = ghc
GHCFLAGS = -O2 -funbox-strict-fields -fexcess-precision -optc-O3

# Test data sizes
SIZES = 100 1000 10000 100000

# Targets
.PHONY: all build test-data run bench-c bench-haskell clean clean-data clean-build

all: build test-data

# Build targets
build: c_benchmark haskell_benchmark

c_benchmark: benchmark.c
	$(CC) $(CFLAGS) -o c_benchmark benchmark.c

haskell_benchmark: Benchmark.hs QuickSort.hs
	$(GHC) $(GHCFLAGS) -o haskell_benchmark Benchmark.hs

# Test data generation
test-data: $(foreach size,$(SIZES),test_data_$(size).txt)

test_data_%.txt:
	@echo "Generating test data for size $*"
	@python3 -c "import random; random.seed(42); [print(random.randint(1, 1000000)) for _ in range($*)]" > $@

# Run both benchmarks sequentially
run: build test-data
	@./run_benchmark.sh c_benchmark
	@./run_benchmark.sh haskell_benchmark

# Individual benchmark targets
bench-c: c_benchmark test-data
	@./run_benchmark.sh c_benchmark

bench-haskell: haskell_benchmark test-data
	@./run_benchmark.sh haskell_benchmark

# Clean targets
clean:
	rm -f c_benchmark haskell_benchmark
	rm -f *.hi *.o
	rm -f test_data_*.txt

clean-data:
	rm -f test_data_*.txt

clean-build:
	rm -f c_benchmark haskell_benchmark
	rm -f *.hi *.o