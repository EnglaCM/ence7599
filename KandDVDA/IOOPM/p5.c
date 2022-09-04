#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

int main(int argc, char *argv[]) {

  if (argc < 1 || argc > 2){
    puts("Måste ange 1 argument");
  } else {

    int n = atoi(argv[1]);
    int limit = floor(sqrt(n)) + 1;

    int x = 2;
    int y = 2;
    bool prim = true;

    for(int i = 1; i < limit; ++i) {

        for (int j = y; j < limit; ++j) {
            if (x*y == n) {
                prim = false;
            } else {

            }

        }

    }

  } 
  return 0;
}
