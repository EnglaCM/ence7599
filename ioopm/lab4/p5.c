#include <stdio.h>

struct point
{
  int x;
  int y;
};

typedef struct point point_t;

struct point translateUnchanged(point_t p1, point_t p2) //förändrar ej p1
{
  p1.x += p2.x;
  p1.y += p2.y;
  return p1;
}

void translate(point_t *p1, point_t *p2) //förändrar p1
{
  p1->x += p2->x;
  p1->y += p2->y;
}

int main(void) {
    struct point p;
    p.x = 10;
    p.y = -42;
    printf("point(x=%d,y=%d)\n", p.x, p.y);

    struct point q = { .x = 10, .y = -42};
    printf("point(x=%d,y=%d)\n", q.x, q.y);

    point_t r = { .x = 10, .y = -42};
    printf("point(x=%d,y=%d)\n", r.x, r.y);

    point_t p1 = { .x = 10 };
    point_t p2 = { .y = -42 };
    point_t p3 = { };

    printf("point(x=%d,y=%d)\n", p1.x, p1.y);
    printf("point(x=%d,y=%d)\n", p2.x, p2.y);
    printf("point(x=%d,y=%d)\n", p3.x, p3.y);

    point_t p4 = translateUnchanged(p1,p2);
    printf("p1: point(x=%d,y=%d)\n", p1.x, p1.y);
    printf("p2: point(x=%d,y=%d)\n", p2.x, p2.y);
    printf("p4: point(x=%d,y=%d)\n", p4.x, p4.y);

    point_t *p5 = &p4;
    point_t *p6 = &p2;

    translate(p5,p6);
    printf("p4: point(x=%d,y=%d)\n", p1.x, p1.y);
    printf("p2: point(x=%d,y=%d)\n", p2.x, p2.y);

    return 0;
}