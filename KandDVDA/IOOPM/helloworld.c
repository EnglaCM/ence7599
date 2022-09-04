#include <stdio.h>

int main(void)
{
  puts("Hello, world!");
  printf("Hello World!\n");

  char *msg = "Hello World";
  int year = 2022;
  printf("%s in the year %d\n", msg, year);

  int x = 1;
  int y = 2;
  printf("x = %d\n", x);
  printf("y = %d\n", y);
  puts("=====");
  int tmp = x;
  x = y;
  y = tmp;
  printf("x = %d\n", x);
  printf("y = %d\n", y);

  int i = 1;              // deklaration och initiering av iterationsvariabeln
  while (i <= 10)         // iterationsvillkor (utför blocket så länge i är mindre än 11)
  {                       // loop-kropp (utförs så länge iterationsvillkoret är uppfyllt)
    printf("%d\n", i);    // skriv ut 1, och en radbrytning
    i = i + 1;            // öka i:s värde med 1 (förändring av iterationsvariabeln)
  }

  for (i = 1; i <= 10; ++i) {
    printf("%d\n", i);
  }

  //6
  int n = 0;
  for (i = 1; i <= 10; ++i) {
    int j = i;
    n = n + i;
    for (j = i; j > 0; j = j-1) {
        printf("*");
    }
    printf("\n");
  }
  printf("Totalt: %d\n", n);

  //7
  

  return 0;
}
