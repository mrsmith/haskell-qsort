CC = gcc
CFLAGS = -O3 -march=native -mtune=native -flto
GHC = ghc
GHCFLAGS = -O2 -funbox-strict-fields -fexcess-precision -optc-O3
SIZES = 100 1000 10000 100000

.PHONY: all build test-data run clean

all: build test-data

build: c_benchmark haskell_benchmark

c_benchmark: benchmark.c
	$(CC) $(CFLAGS) -o $@ $<

haskell_benchmark: Benchmark.hs QuickSort.hs
	$(GHC) $(GHCFLAGS) -o $@ $<

test-data: $(foreach size,$(SIZES),test_data_$(size).txt)

test_data_%.txt:
	@python3 -c "import random; random.seed(42); [print(random.randint(1, 1000000)) for _ in range($*)]" > $@

run: build test-data
	@./run_benchmark.sh c_benchmark
	@./run_benchmark.sh haskell_benchmark

clean:
	rm -f c_benchmark haskell_benchmark *.hi *.o test_data_*.txt