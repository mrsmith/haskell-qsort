#!/bin/bash

# Compile the Haskell benchmark program
ghc -O2 -o haskell_benchmark Benchmark.hs

if [ $? -ne 0 ]; then
    echo "Failed to compile Haskell benchmark"
    exit 1
fi

# Test sizes
sizes=(100 1000 10000 100000)

echo "Haskell Quicksort Benchmark Results"
echo "==================================="
printf "%-10s %-15s %-15s %-10s\n" "Size" "Time (seconds)" "Elements/sec" "Status"
echo "--------------------------------------------------------"

for size in "${sizes[@]}"; do
    # Generate random numbers
    python3 -c "import random; [print(random.randint(1, 1000000)) for _ in range($size)]" > test_data_$size.txt
    
    # Run benchmark
    result=$(./haskell_benchmark < test_data_$size.txt)
    
    # Extract time and status
    time_taken=$(echo "$result" | grep "Time taken:" | awk '{print $3}')
    status=$(echo "$result" | grep "Sort verified:" | awk '{print $3}')
    
    # Calculate elements per second
    if [ "$time_taken" != "0.000000" ]; then
        elements_per_sec=$(python3 -c "print(f'{$size / $time_taken:.0f}')")
    else
        elements_per_sec="N/A"
    fi
    
    printf "%-10s %-15s %-15s %-10s\n" "$size" "$time_taken" "$elements_per_sec" "$status"
    
    # Clean up test file
    rm test_data_$size.txt
done

# Clean up executable and intermediate files
rm haskell_benchmark
rm -f *.hi *.o

echo ""
echo "Haskell benchmark completed!"