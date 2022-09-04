#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int fib(int numbers, int x, int y) {
    //printf("x: %d    y: %d\n", x, y);

    if (numbers > 1) {
        x = x+y;
        return x + fib(numbers-1, y, x);
    } else {
        return 1;
    }
}

int main(void) {
    int res = fib(5, 1, 1);
    printf("%d\n", res);

    return 0;
}