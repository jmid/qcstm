// Queue example from John Hughes
// "Experiences with QuickCheck: Testing the Hard Stuff and Staying Sane"
#include  <stdlib.h>

typedef struct queue
{ int *buf;
  int inp, outp, size;
} Queue;

Queue *new(int n)
{ int *buff = malloc((n+1) * sizeof(int)); //fix
  Queue q = {buff, 0, 0, n+1};             //fix
  Queue *qptr = malloc(sizeof(Queue));
  *qptr = q;
  return qptr; }

void put(Queue *q, int n)
{ q -> buf[q -> inp] = n;
  q -> inp = (q -> inp + 1) % q->size; }

int get(Queue *q)
{ int ans = q -> buf[q -> outp];
  q -> outp = (q -> outp + 1) % q->size;
  return ans; }

int size(Queue *q)
{ return (q->inp - q->outp + q->size) % q->size; } //fix
