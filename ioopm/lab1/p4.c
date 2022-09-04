#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
  /*
  for (int i = 0; i < argc; ++i)
  {
    puts(argv[i]);
  }

  char *str = "42";
  int num = atoi(str);
  printf("%s == %d?\n", str, num);
  */

  if (argc < 2 || argc > 3){
    puts("Måste ange 2 argument");
  } else {

    int rows = atoi(argv[1]);
    int jumps = atoi(argv[2]);
    int n = jumps;
    int tot = 0;


    for (int i = 1; i <= rows; i = ++i) {


      for (int j = 1; j <= n; j = j+1) {
          printf("*");

          tot = tot + 1;
      }
      //printf("%d\n", n);
      n = n + jumps;
      //printf("%d\n", n);
      

      printf("\n");
    
    }
    printf("Totalt: %d\n", tot);
  } 
  return 0;
}
