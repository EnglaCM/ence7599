#include <stdio.h>

void string_copy(char *source, char *dest)
{
  while ((*dest++ = *source++)) ;
}

int string_length(char *str)
{
  char *end = str;
  while (*end != '\0') ++end;
  return end - str;
}

int main(void) {
    char *a;
    a = "hej";
    int b = string_length(a);

    char c[b]; // Är detta rätt? Ska man skriva hur lång c är? Annars fick jag "‘c’ is used uninitialized in this function"
    string_copy(a,c);

    printf("%s   %d\n", c, b);
}