#include "keshav_isa.h"

int main() {
    volatile long arr[4];
    arr[0] = 64;
    arr[1] = 25;
    arr[2] = 12;
    arr[3] = 5;

    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3 - i; j++) {
            long a = arr[j];
            long b = arr[j + 1];
            if (a > b) {
                long min_val, max_val;
                KESHAV_MIN(min_val, a, b);
                KESHAV_MAX(max_val, a, b);
                arr[j]     = min_val;
                arr[j + 1] = max_val;
            }
        }
    }

    return arr[0]; // Expected: 5
}
