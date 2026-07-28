int main() {
    volatile int A[2][2];
    volatile int B[2][2];
    volatile int C[2][2];

    A[0][0] = 12; A[0][1] = 7;
    A[1][0] = 5;  A[1][1] = 9;

    B[0][0] = 8;  B[0][1] = 15;
    B[1][0] = 23; B[1][1] = 4;

    for (int i = 0; i < 2; i++) {
        for (int j = 0; j < 2; j++) {
            C[i][j] = 0;
            for (int k = 0; k < 2; k++) {
                C[i][j] += A[i][k] * B[k][j];
            }
        }
    }

    int total_sum = C[0][0] + C[0][1] + C[1][0] + C[1][1];
    return total_sum; // Expected: (96+161) + (180+28) + (40+207) + (75+36) = 257 + 208 + 247 + 111 = 823
}
