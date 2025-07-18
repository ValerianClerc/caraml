#include <stdio.h>
#include <stdlib.h>

void printint(int i)
{
  printf("%d", i);
}

void printbool(int b)
{
  if (b) {
    printf("true");
  } else {
    printf("false");
  }
}