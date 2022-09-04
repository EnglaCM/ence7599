#include "utils.h" //KOM IHÅG
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

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

int clear_input_buffer(void) {
    int c;
    do
    {
      c = getchar();
    }
    while (c != '\n');

    return 0;
}

int read_string(char *buf, int buf_siz) {
    int read = 0;
    int clear = 1;
    char c;

    for(int i = 0; i < buf_siz-1; i++)
    {
        c = getchar();
        if(c == '\n' || c == '\0' || c == EOF) // If we get to line break, end of string or end of file, we've read all we need within the buffert size
        {
            // Read the entire string. No need to clear the input buffer
            clear = 0;
            break;
        }
        buf[i] = c;
        read++;
    }

    if(clear){ // If we didn't reach the end of line/string/file, we will still have input left in the buffert, and this needs to be cleared, so that the program doesn't read the rest as the next input
        clear_input_buffer();
    }
    
    buf[read] = '\0'; // Always add '\0' at the end of a built string to mark the end of the string

    return read;
}

bool is_number(char *str)
{
    int n = 0;
    if (str[0] == '-' && strlen(str) > 1) {
        n = 1;
    }

    for (int i = n; i < strlen(str); i++) {
        if (!isdigit(str[i])) {
            return false;
        }
    }
    return true;
}

bool not_empty(char *str)
{
  return strlen(str) > 0;
}

answer_t ask_question(char *question, check_func check, convert_func convert) {
    int buf_siz = 255;
    char buf[buf_siz];
    int read = 0;

    do {
        printf("%s\n", question);
        read = read_string(buf, buf_siz);
    }
    while (read == 0 || !check(buf));

    answer_t res = convert(buf);

    return res;
}

int ask_question_int(char *question)
{
  answer_t answer = ask_question(question, is_number, (convert_func) atoi);
  return answer.int_value; // svaret som ett heltal
}

char *ask_question_string(char *question)
{
  return ask_question(question, not_empty, (convert_func) strdup).string_value;
}