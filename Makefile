CC = gcc
CFLAGS = -O3 -march=native -mtune=native -flto
GHC = ghc
GHCFLAGS = -O2 -funbox-strict-fields -fexcess-precision -optc-O3
SIZES = 100 1000 10000 100000
DATA_TYPES = random sorted reverse duplicates

.PHONY: all build test-data run clean

all: build test-data

build: c_benchmark qsort_benchmark haskell_benchmark stdsort_benchmark inplace_benchmark

c_benchmark: benchmark.c
	$(CC) $(CFLAGS) -o $@ $<

qsort_benchmark: qsort_benchmark.c
	$(CC) $(CFLAGS) -o $@ $<

haskell_benchmark: Benchmark.hs QuickSort.hs
	$(GHC) $(GHCFLAGS) -o $@ $<

stdsort_benchmark: StdSort.hs
	$(GHC) $(GHCFLAGS) -o $@ $<

inplace_benchmark: InPlaceBenchmark.hs InPlaceSort.hs
	$(GHC) $(GHCFLAGS) -o $@ $<

test-data: data $(foreach type,$(DATA_TYPES),$(foreach size,$(SIZES),data/$(type)_$(size).txt))

data:
	@mkdir -p data

data/random_%.txt:
	@python3 -c "import random; random.seed(42); [print(random.randint(1, 1000000)) for _ in range($*)]" > $@

data/sorted_%.txt:
	@python3 -c "[print(i) for i in range(1, $*+1)]" > $@

data/reverse_%.txt:
	@python3 -c "[print(i) for i in range($*, 0, -1)]" > $@

data/duplicates_%.txt:
	@python3 -c "import random; random.seed(42); vals=[1,2,3,4,5]; [print(random.choice(vals)) for _ in range($*)]" > $@

run: build test-data
	@echo "=== RANDOM DATA ==="
	@./run_benchmark.sh c_benchmark random
	@./run_benchmark.sh qsort_benchmark random
	@./run_benchmark.sh haskell_benchmark random
	@./run_benchmark.sh inplace_benchmark random
	@./run_benchmark.sh stdsort_benchmark random
	@echo
	@echo "=== DUPLICATES DATA ==="
	@./run_benchmark.sh c_benchmark duplicates
	@./run_benchmark.sh qsort_benchmark duplicates
	@./run_benchmark.sh haskell_benchmark duplicates
	@./run_benchmark.sh inplace_benchmark duplicates
	@./run_benchmark.sh stdsort_benchmark duplicates

clean:
	rm -f c_benchmark qsort_benchmark haskell_benchmark stdsort_benchmark inplace_benchmark *.hi *.o
	rm -rf data
	rm -f test_data_*.txt