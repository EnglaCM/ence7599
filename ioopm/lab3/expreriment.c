#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

extern char *strdup(const char *);


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
        if(c == '\n' || c == '\0' || c == EOF)
        {
            // Read the entire string. No need to clear the input buffer
            clear = 0;
            break;
        }
        buf[i] = c;
        read++;
    }

    if(clear){
        clear_input_buffer();
    }
    
    buf[read] = '\0';

    return read;
}



bool is_number(char *str)
{   bool is_num = true;
    int n = 0;
    if (str[0] == '-' && strlen(str) > 1) {
        n = 1;
    }

    for (int i = n; i < strlen(str); i++) {
        if (!isdigit(str[i])) {
            is_num = false;
        }
    }
    return is_num;
}



typedef union {
    int int_value;
    float float_value;
    char *string_value;
} answer_t; // answer_t är antingen int, float eller sträng

typedef bool(*check_func)(char *);

typedef answer_t(*convert_func)(char *);


answer_t ask_question(char *question, check_func check, convert_func convert) {
    int buf_siz = 255;
    char buf[buf_siz];
    bool val = false;
    int read = 0;

    do {
        printf("%s\n", question);
        read = read_string(buf, buf_siz);
        if (check(buf)) {
            val = true;
        }
    }
    while (read == 0 || val == false);

    answer_t res = convert(buf);

    return res;
}

int ask_question_int(char *question)
{
  answer_t answer = ask_question(question, is_number, (convert_func) atoi);
  return answer.int_value; // svaret som ett heltal
}

bool not_empty(char *str)
{
  return strlen(str) > 0;
}

char *ask_question_string(char *question)
{
  return ask_question(question, not_empty, (convert_func) strdup).string_value;
}

int main(void) {
    int tal = ask_question_int("write number:");
    printf("%d\n",tal);

    char *str = ask_question_string("write str:");
    printf("%s\n",str);

    return 0;
}