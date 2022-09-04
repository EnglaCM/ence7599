#include <stdio.h>

void swapWrong(int a, int b)
{
  int tmp = a;
  a = b;
  b = tmp;
}

void swap(int *a, int *b)
{
  int tmp = *a;
  *a = *b;
  *b = tmp;
}

int main(void)
{
  // p1
  int x = 7;
  int y = 42;
  swapWrong(x, y);
  printf("%d, %d\n", x, y); //prints 7, 42
  // x och y skickas bara som värden till funktionen. 
  //Om de skickades som pekare skulle värdena på x och y ändras även i main, 
  //men nu har a och b i swap sin egen plats i minnet och är alltså inte samma som x och y

  //"a och b har samma värde som x och y initialt, men förändringar av a och b påverkar inte x och y" - labbinstruktionerna


  //p2
  int  a;  // a är en variabel som innehåller ett heltal
  int *b;  // b är en variabel som innehåller en adress till en plats i minnet där det finns ett heltal

  a = 42;  // a innehåller nu heltalet 42
  b = &a;  // b innehåller nu en adress till där värdet på a finns lagrat
  printf("%d, %d\n", a, *b);  // skriver ut 42, 42
  a = 7;
  printf("%d, %d\n", a, *b);  // skriver ut 7, 7
  *b = 42;
  printf("%d, %d\n", a, *b);  // skriver ut 42, 42

  //vid initieringen anges det att c och d är pekare
  int *c; 
  int *d; 

  // när c och d ska få sina värden skrivs inte * ut, men ett & ska vara framför variabeln vars minne c och d ska peka på
  // * kallas för avrefereringsoperatorn 
  // & kallas för adresstagningsoperatorn 
  c = &x;
  d = &y;
  swap(c, d);
  printf("%d, %d\n", x, y);

  return 0;
}