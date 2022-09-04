#include "utils.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

int main(void) {
    srand(time(NULL));

    int buf_siz = 255;
    char buf[buf_siz];
    int tal = random() % 1024;
    
    char *namn = ask_question_string("Skriv ditt namn:", buf, buf_siz);

    printf("Du %s, ", namn);
    int gissning = ask_question_int("jag tänker på ett tal ... kan du gissa vilket?");
    int gissningar = 1;


    if (gissning != tal) {
        for (int i = 0; i < 14; i++) {
            if (gissning < tal) {
                gissning = ask_question_int("För litet!");
                gissningar++;
            } else if (gissning > tal) {
                gissning = ask_question_int("För stort!");
                gissningar++;
            } else {
                puts("Bingo!");
                printf("Det tog %s %d gissningar att komma fram till %d\n", namn, gissningar, tal);
                return 0;
            }
        }
    }

    if (gissning == tal) {
        puts("Bingo!");
        printf("Det tog %s %d gissningar att komma fram till %d\n", namn, gissningar, tal);
        return 0;
    }

    printf("Nu har du slut på gissningar! Jag tänkte på %d!", tal);

    return 0;
}