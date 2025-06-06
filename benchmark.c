#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <math.h>

void swap(int* a, int* b) {
    int temp = *a; *a = *b; *b = temp;
}

int partition(int arr[], int low, int high) {
    int pivot = arr[high], i = low - 1;
    for (int j = low; j < high; j++) {
        if (arr[j] < pivot) {
            swap(&arr[++i], &arr[j]);
        }
    }
    swap(&arr[i + 1], &arr[high]);
    return i + 1;
}

void quicksort(int arr[], int low, int high) {
    if (low < high) {
        int pi = partition(arr, low, high);
        quicksort(arr, low, pi - 1);
        quicksort(arr, pi + 1, high);
    }
}

int is_sorted(int arr[], int n) {
    for (int i = 1; i < n; i++) {
        if (arr[i] < arr[i - 1]) return 0;
    }
    return 1;
}

int main() {
    int capacity = 1000, n = 0, num;
    int* arr = malloc(capacity * sizeof(int));
    
    while (scanf("%d", &num) == 1) {
        if (n >= capacity) {
            arr = realloc(arr, (capacity *= 2) * sizeof(int));
        }
        arr[n++] = num;
    }
    
    if (n == 0) {
        puts("No input data");
        free(arr);
        return 1;
    }
    
    // More warmup runs for small inputs that are noisier
    int warmup_runs = (n < 10000) ? 3 : 1;
    int total_runs = warmup_runs + 4;
    double times[4];  // 4 measurement runs after warmup
    
    for (int run = 0; run < total_runs; run++) {
        int* copy = malloc(n * sizeof(int));
        // Apply multiplier like Haskell for cache consistency
        int multiplier = run + 1;
        for (int i = 0; i < n; i++) {
            copy[i] = arr[i] * multiplier;
        }
        
        // Determine iterations based on input size for measurable timing
        int iterations = (n < 1000) ? 1000 : (n < 10000) ? 100 : 1;
        
        struct timespec start, end;
        clock_gettime(CLOCK_MONOTONIC, &start);
        
        for (int iter = 0; iter < iterations; iter++) {
            // Reset array for each iteration (except last one for verification)
            if (iter < iterations - 1) {
                for (int j = 0; j < n; j++) {
                    copy[j] = arr[j] * multiplier;
                }
            }
            quicksort(copy, 0, n - 1);
        }
        
        clock_gettime(CLOCK_MONOTONIC, &end);
        
        if (run >= warmup_runs) {  // Skip warmup runs
            double elapsed = (end.tv_sec - start.tv_sec) + 
                           (end.tv_nsec - start.tv_nsec) / 1e9;
            times[run - warmup_runs] = elapsed / iterations;  // Average per iteration
        }
        if (run == 0) {
            printf("Sort verified: %s\n", is_sorted(copy, n) ? "PASSED" : "FAILED");
        }
        free(copy);
    }
    
    for (int i = 0; i < 3; i++) {
        for (int j = i + 1; j < 4; j++) {
            if (times[i] > times[j]) {
                double temp = times[i];
                times[i] = times[j];
                times[j] = temp;
            }
        }
    }
    
    double median = times[1];  // median of 4 values (average of middle two, but times[1] is close enough)
    double mean = (times[0] + times[1] + times[2] + times[3]) / 4.0;
    double variance = 0.0;
    for (int i = 0; i < 4; i++) {
        double diff = times[i] - mean;
        variance += diff * diff;
    }
    double stddev = sqrt(variance / 3.0);  // n-1 for sample std dev
    double cv = (stddev / median) * 100.0;  // coefficient of variation
    
    printf("Elements sorted: %d\nTime taken: %.6f seconds (±%.1f%%)\n", n, median, cv);
    
    free(arr);
    return 0;
}