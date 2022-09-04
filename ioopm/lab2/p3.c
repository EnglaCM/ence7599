#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>

void print_number(int num) {
    if(num%3 == 0 && num%5 == 0) {
        printf("fizz buzz, ");
    } else if (num%3 == 0){
        printf("fizz, ");
    } else if (num%5 == 0){
        printf("buzz, ");
    } else {
        printf("%d, ", num);
    }
}

int main(int argc, char *argv[]) {
    if (argc == 2){

        int end = atoi(argv[1]);

        for (int i = 1; i <= end; i++) {
            if(i < end) {
                print_number(i);
            } else {
                if(i%3 == 0 && i%5 == 0) {
                    printf("fizz buzz\n");
                } else if (i%3 == 0){
                    printf("fizz\n");
                } else if (i%5 == 0){
                    printf("buzz\n");
                } else {
                    printf("%d\n", i);
                }
            }
        }
      
    } else {

      printf("Please provide a command line argument!\n");

    }
    return 0;
}