#include <stdio.h>
#include <string.h>
#include <ctype.h>

void print(char *str)
{
  char *c = str;
  while (*c != '\0') {
      putchar(*c);
      ++c;
  }
}

void println(char *str)
{
  char *c = str;
  while (*c != '\0') {
      putchar(*c);
      ++c;
  }
  putchar('\n');
}

char *trim(char *str)
{
  char *start = str;
  char *end = start + strlen(str)-1;

  while (isspace(*start)) {
      ++start; 
  }
  while (isspace(*end)) {
      --end;
  }

  char *cursor = str;
  for (; start <= end; ++start, ++cursor)
    {
      *cursor = *start;
    }
  *cursor = '\0';

  return str;
}

int main(void) {
    char *str = "hej\n";
    print(str);

    char str1[] = "  hej  ";
    char str2[] = "  h ej  ";
    char str3[] = "  hej\t ";
    char str4[] = "  hej\t \n";

    char *tests[] = { str1, str2, str3, str4 };

    for (int i = 0; i < 4; ++i)
      {
        print("Utan trim: '");
        print(tests[i]);
        print("'\nMed trim:  '");
        print(trim(tests[i]));
        println("'\n");
    }

    return 0;
}