#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

void swap(int* a, int* b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}

int partition(int arr[], int low, int high) {
    int pivot = arr[high];
    int i = (low - 1);
    
    for (int j = low; j <= high - 1; j++) {
        if (arr[j] < pivot) {
            i++;
            swap(&arr[i], &arr[j]);
        }
    }
    swap(&arr[i + 1], &arr[high]);
    return (i + 1);
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
        if (arr[i] < arr[i - 1]) {
            return 0;
        }
    }
    return 1;
}

int main() {
    int capacity = 1000;
    int* arr = malloc(capacity * sizeof(int));
    int* original = malloc(capacity * sizeof(int));
    int n = 0;
    int num;
    
    while (scanf("%d", &num) == 1) {
        if (n >= capacity) {
            capacity *= 2;
            arr = realloc(arr, capacity * sizeof(int));
            original = realloc(original, capacity * sizeof(int));
        }
        arr[n] = num;
        original[n] = num;
        n++;
    }
    
    if (n == 0) {
        printf("No input data\n");
        free(arr);
        free(original);
        return 1;
    }
    
    clock_t start = clock();
    quicksort(arr, 0, n - 1);
    clock_t end = clock();
    
    double time_taken = ((double)(end - start)) / CLOCKS_PER_SEC;
    
    if (is_sorted(arr, n)) {
        printf("Sort verified: PASSED\n");
    } else {
        printf("Sort verified: FAILED\n");
    }
    
    printf("Elements sorted: %d\n", n);
    printf("Time taken: %.6f seconds\n", time_taken);
    
    free(arr);
    free(original);
    return 0;
}