#include <stdio.h>
#include <string.h>

int string_length(char *str) {
    int len = 0;
    for (int i = 0; str[i] != '\0'; i++) {
        len++;
    }

    return len;
}

void print(char *str) {
    for (int i = 0; str[i] != '\0'; i++) {
        putchar(str[i]);
    }

}

void println(char *str) {
    for (int i = 0; str[i] != '\0'; i++) {
        putchar(str[i]);
    }
    putchar('\n');
}

int main(int argc, char *argv[])
{
  if (argc < 2)
  {
    printf("Usage: %s words or string", argv[0]);
  }
  else
  {
    for (int i = 1; i < argc; ++i)
    {
      int expected = strlen(argv[i]);
      int actual   = string_length(argv[i]);
      printf("strlen(\"%s\")=%d\t\tstring_length(\"%s\")=%d\n",
             argv[i], expected, argv[i], actual);
      print(argv[i]);
      println(argv[i]);
    }
  }
  return 0;
}