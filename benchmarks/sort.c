int main() {
    volatile int arr[8];
    arr[0] = 64; arr[1] = 34; arr[2] = 25; arr[3] = 12;
    arr[4] = 22; arr[5] = 11; arr[6] = 90; arr[7] = 5;

    for (int i = 0; i < 7; i++) {
        for (int j = 0; j < 7 - i; j++) {
            if (arr[j] > arr[j + 1]) {
                int temp = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = temp;
            }
        }
    }
    return arr[0];
}
