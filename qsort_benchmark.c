#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <math.h>

double now_ns() {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec + ts.tv_nsec / 1e9;
}

double median(double times[], int n) {
    for (int i = 0; i < n-1; i++) {
        for (int j = i+1; j < n; j++) {
            if (times[i] > times[j]) {
                double temp = times[i];
                times[i] = times[j];
                times[j] = temp;
            }
        }
    }
    return times[n/2];
}

double coefficient_of_variation(double times[], int n, double mean_time) {
    double variance = 0.0;
    for (int i = 0; i < n; i++) {
        double diff = times[i] - mean_time;
        variance += diff * diff;
    }
    double stddev = sqrt(variance / (n-1));
    return (stddev / mean_time) * 100.0;
}

double calculate_mean(double times[], int n) {
    double sum = 0.0;
    for (int i = 0; i < n; i++) sum += times[i];
    return sum / n;
}

int compare_ints(const void *a, const void *b) {
    return (*(int*)a - *(int*)b);
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
    
    // Unified methodology: 1 warmup + 5 measurement runs
    double times[5];
    
    for (int run = 0; run < 6; run++) {
        int* copy = malloc(n * sizeof(int));
        int multiplier = run + 1;  // Same multiplier approach as Haskell
        
        for (int i = 0; i < n; i++) {
            copy[i] = arr[i] * multiplier;
        }
        
        // Add iterations for small inputs to get measurable timing
        int iterations = (n < 1000) ? 1000 : (n < 10000) ? 100 : 1;
        
        double start = now_ns();
        for (int iter = 0; iter < iterations; iter++) {
            if (iter > 0) {
                // Reset array for each iteration (except last one for verification)
                for (int j = 0; j < n; j++) {
                    copy[j] = arr[j] * multiplier;
                }
            }
            qsort(copy, n, sizeof(int), compare_ints);
        }
        double elapsed = (now_ns() - start) / iterations;
        
        if (run > 0) {  // Skip first run (warmup)
            times[run - 1] = elapsed;
        }
        if (run == 0) {
            printf("Sort verified: %s\n", is_sorted(copy, n) ? "PASSED" : "FAILED");
        }
        free(copy);
    }
    
    double median_time = median(times, 5);
    double mean_time = calculate_mean(times, 5);
    double cv = coefficient_of_variation(times, 5, mean_time);
    
    printf("Elements sorted: %d\nMedian: %.6f seconds, Mean: %.6f seconds (±%.1f%%)\n", n, median_time, mean_time, cv);
    
    free(arr);
    return 0;
}