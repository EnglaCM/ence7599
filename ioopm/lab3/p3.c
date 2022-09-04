#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef int(*int_fold_func)(int, int);

/// En funktion som tar en array av heltal, arrayens längd och
/// en pekare till en funktion f av typen Int -> Int -> Int
int foldl_int_int(int numbers[], int numbers_siz, int_fold_func f)
{
  int result = 0;

  // Loopa över arrayen och för varje element e utför result = f(result, e)
  for (int i = 0; i < numbers_siz; ++i)
  {
    result = f(result, numbers[i]);
  }

  return result;
}


// Låt oss skriva en funktion som adderar två tal:

int add(int a, int b)
{
  return a + b;
}

long sum(int numbers[], int numbers_siz)
{

    return foldl_int_int(numbers, numbers_siz, add);
}

int main(void) {

    int numbers[3];
    numbers[0] = 1;
    numbers[1] = 3;
    numbers[2] = 8;
    int tal = sum(numbers, 3);
    printf("tal: %d\n", tal);
    
    return 0;
}