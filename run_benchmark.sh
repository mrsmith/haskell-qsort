#!/bin/bash
[ $# -ne 2 ] || [ ! -f "$1" ] && { echo "Usage: $0 <binary> <data_type>"; exit 1; }

binary="$1"
data_type="$2"

echo "Quicksort Benchmark Results ($binary - $data_type data)"
echo "========================================================="
printf "%-10s %-15s %-15s %-15s %-10s\n" "Size" "Median (s)" "Mean (s)" "Elements/sec" "Status"
echo "------------------------------------------------------------------------"

for size in 100 1000 10000 100000; do
    data_file="data/${data_type}_${size}.txt"
    [ ! -f "$data_file" ] && { echo "Error: $data_file not found"; exit 1; }
    
    # Add timeout for worst-case scenarios (sorted data can trigger O(n²))
    # Note: Using gtimeout (GNU coreutils) or fallback to direct execution
    if command -v gtimeout >/dev/null 2>&1; then
        result=$(gtimeout 30s ./"$binary" < "$data_file" 2>/dev/null)
        if [ $? -eq 124 ]; then
            printf "%-10s %-15s %-15s %-15s %-10s\n" "$size" "TIMEOUT" "TIMEOUT" "N/A" "TIMEOUT"
            continue
        fi
    else
        # Fallback: no timeout protection (may hang on worst cases)
        result=$(./"$binary" < "$data_file" 2>/dev/null)
    fi
    
    time_line=$(echo "$result" | grep "Median:")
    if [ -z "$time_line" ]; then
        printf "%-10s %-15s %-15s %-15s %-10s\n" "$size" "ERROR" "ERROR" "N/A" "ERROR"
        continue
    fi
    
    median=$(echo "$time_line" | awk '{print $2}')
    mean=$(echo "$time_line" | awk '{print $5}')
    variance=$(echo "$time_line" | sed 's/.*±\([0-9.]*\)%.*/±\1%/')
    status=$(echo "$result" | awk '/Sort verified:/ {print $3}')
    
    rate=$(python3 -c "print('N/A' if '$mean' == '' or float('$mean') == 0 else f'{$size/float('$mean'):.0f}')")
    printf "%-10s %-15s %-15s %-15s %-10s\n" "$size" "$median $variance" "$mean" "$rate" "$status"
done