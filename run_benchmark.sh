#!/bin/bash
[ $# -ne 1 ] || [ ! -f "$1" ] && { echo "Usage: $0 <binary>"; exit 1; }

echo "Quicksort Benchmark Results ($1)"
echo "======================================"
printf "%-10s %-20s %-15s %-10s\n" "Size" "Time (seconds)" "Elements/sec" "Status"
echo "----------------------------------------------------------------"

for size in 100 1000 10000 100000; do
    [ ! -f "test_data_$size.txt" ] && { echo "Error: test_data_$size.txt not found"; exit 1; }
    
    result=$(./"$1" < test_data_$size.txt)
    time_line=$(echo "$result" | grep "Time taken:")
    time=$(echo "$time_line" | awk '{print $3}')
    variance=$(echo "$time_line" | sed 's/.*±\([0-9.]*\)%.*/±\1%/')
    status=$(echo "$result" | awk '/Sort verified:/ {print $3}')
    
    rate=$(python3 -c "print('N/A' if $time == 0 else f'{$size/$time:.0f}')")
    printf "%-10s %-20s %-15s %-10s\n" "$size" "$time $variance" "$rate" "$status"
done