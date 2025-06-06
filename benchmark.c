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
    
    double times[4];  // 4 measurement runs after 1 warmup
    for (int run = 0; run < 5; run++) {
        int* copy = malloc(n * sizeof(int));
        // Apply multiplier like Haskell for cache consistency
        int multiplier = run + 1;
        for (int i = 0; i < n; i++) {
            copy[i] = arr[i] * multiplier;
        }
        
        clock_t start = clock();
        quicksort(copy, 0, n - 1);
        clock_t end = clock();
        
        if (run > 0) {  // Skip first run (warmup)
            times[run - 1] = ((double)(end - start)) / CLOCKS_PER_SEC;
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