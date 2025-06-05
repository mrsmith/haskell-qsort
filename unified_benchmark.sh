#!/bin/bash

# Compile both programs
echo "Compiling programs..."
gcc -O2 -o c_benchmark benchmark.c
if [ $? -ne 0 ]; then
    echo "Failed to compile C benchmark"
    exit 1
fi

ghc -O2 -o haskell_benchmark Benchmark.hs
if [ $? -ne 0 ]; then
    echo "Failed to compile Haskell benchmark"
    exit 1
fi

# Test sizes
sizes=(100 1000 10000 100000)

echo ""
echo "Quicksort Benchmark Comparison"
echo "=============================="
echo ""

# Generate test data files once
echo "Generating test data..."
for size in "${sizes[@]}"; do
    python3 -c "import random; random.seed(42); [print(random.randint(1, 1000000)) for _ in range($size)]" > test_data_$size.txt
done

echo ""
printf "%-10s %-15s %-15s %-15s %-15s %-10s %-10s\n" "Size" "C Time (s)" "C Elem/s" "Haskell Time (s)" "Haskell Elem/s" "C Status" "HS Status"
echo "------------------------------------------------------------------------------------------------------"

for size in "${sizes[@]}"; do
    # Run C benchmark
    c_result=$(./c_benchmark < test_data_$size.txt)
    c_time=$(echo "$c_result" | grep "Time taken:" | awk '{print $3}')
    c_status=$(echo "$c_result" | grep "Sort verified:" | awk '{print $3}')
    
    # Calculate C elements per second
    if [ "$c_time" != "0.000000" ]; then
        c_elem_per_sec=$(python3 -c "print(f'{$size / $c_time:.0f}')")
    else
        c_elem_per_sec="N/A"
    fi
    
    # Run Haskell benchmark
    hs_result=$(./haskell_benchmark < test_data_$size.txt)
    hs_time=$(echo "$hs_result" | grep "Time taken:" | awk '{print $3}')
    hs_status=$(echo "$hs_result" | grep "Sort verified:" | awk '{print $3}')
    
    # Calculate Haskell elements per second
    if [ "$hs_time" != "0.000000" ]; then
        hs_elem_per_sec=$(python3 -c "print(f'{$size / $hs_time:.0f}')")
    else
        hs_elem_per_sec="N/A"
    fi
    
    printf "%-10s %-15s %-15s %-15s %-15s %-10s %-10s\n" "$size" "$c_time" "$c_elem_per_sec" "$hs_time" "$hs_elem_per_sec" "$c_status" "$hs_status"
done

echo ""
echo "Performance Ratio (C/Haskell):"
echo "------------------------------"
for size in "${sizes[@]}"; do
    c_result=$(./c_benchmark < test_data_$size.txt)
    c_time=$(echo "$c_result" | grep "Time taken:" | awk '{print $3}')
    
    hs_result=$(./haskell_benchmark < test_data_$size.txt)
    hs_time=$(echo "$hs_result" | grep "Time taken:" | awk '{print $3}')
    
    if [ "$c_time" != "0.000000" ] && [ "$hs_time" != "0.000000" ]; then
        ratio=$(python3 -c "print(f'{$hs_time / $c_time:.1f}x')")
        printf "Size %-10s: Haskell is %s slower than C\n" "$size" "$ratio"
    fi
done

# Clean up
echo ""
echo "Cleaning up..."
rm c_benchmark haskell_benchmark
rm -f *.hi *.o
for size in "${sizes[@]}"; do
    rm test_data_$size.txt
done

echo "Benchmark completed!"