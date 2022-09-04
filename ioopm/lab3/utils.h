#ifndef __UTILS_H__
#define __UTILS_H__

#include <stdbool.h>

extern char *strdup(const char *);

typedef union {
    int int_value;
    float float_value;
    char *string_value;
} answer_t; // answer_t är antingen int, float eller sträng

typedef bool(*check_func)(char *);

typedef answer_t(*convert_func)(char *);
char *ask_question_string(char *question);
int ask_question_int(char *question);
answer_t ask_question(char *question, check_func check, convert_func convert);
bool not_empty(char *str);
bool is_number(char *str);
int read_string(char *buf, int buf_siz);


#endif 