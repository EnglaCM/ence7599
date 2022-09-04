#include "utils.h"
#include <stdio.h>
#include <string.h>

int ask_question_int(char *question) {
    int result = 0;
    int conversions = 0;
    do
      {
        printf("%s\n", question);
        conversions = scanf("%d", &result);
        int c;
        do
          {
            c = getchar();
          }
        while (c != '\n' && c != EOF);
        putchar('\n');
      }
    while (conversions < 1);
    return result;
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

    /* Won't work since the "while-check" doesn't break the do-loop if the condition is false, 
    it just prevent it from going into the do-loop one more time
    
    do {
        c = getchar();
        buf[read] = c;
        read++;
    }
    while (c != '\n' && c != EOF && c !='\0' && read < buf_siz);
    */

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

char *ask_question_string(char *question, char *buf, int buf_siz)
{   
    int length = 0;
    do
        {
            printf("%s\n", question);
            length = read_string(buf, buf_siz);
        }
    while (length == 0);
    return strdup(buf);
}

/*

int main(void) {
    int tal;

    tal = ask_question_int("Första talet:");
    printf("Du skrev '%d'\n", tal);

    tal = ask_question_int("Andra talet:");
    printf("Du skrev '%d'\n", tal);

    //ask_question_string("Fråga");
    
    int buf_siz = 255;
    int read = 0;
    char buf[buf_siz];

    puts("Läs in en sträng:");
    read = read_string(buf, buf_siz);
    printf("'%s' (%d tecken)\n", buf, read);

    puts("Läs in en sträng till:");
    read = read_string(buf, buf_siz);
    printf("'%s' (%d tecken)\n", buf, read);

    ask_question_string("Question", buf, buf_siz);
    

    return 0;

}
*/