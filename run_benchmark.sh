#!/bin/bash

if [ $# -ne 1 ]; then
    echo "Usage: $0 <binary>"
    echo "Example: $0 c_benchmark"
    echo "Example: $0 haskell_benchmark"
    exit 1
fi

BINARY=$1

if [ ! -f "$BINARY" ]; then
    echo "Error: Binary '$BINARY' not found"
    exit 1
fi

# Test sizes
sizes=(100 1000 10000 100000)

echo "Quicksort Benchmark Results ($BINARY)"
echo "======================================"
printf "%-10s %-15s %-15s %-10s\n" "Size" "Time (seconds)" "Elements/sec" "Status"
echo "--------------------------------------------------------"

for size in "${sizes[@]}"; do
    if [ ! -f "test_data_$size.txt" ]; then
        echo "Error: test_data_$size.txt not found. Run 'make test-data' first."
        exit 1
    fi
    
    # Run benchmark
    result=$(./"$BINARY" < test_data_$size.txt)
    
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
done

echo ""