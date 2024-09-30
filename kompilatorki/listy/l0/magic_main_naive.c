#include <stdio.h>

static int magic(int y) {
  int sum = 0, x = 1;
  while (x > 0) {
    sum += x ^ y;
    y *= 13;
    x += x / 2 + 1;
  }
  return sum * 42;
}

int main() {
  printf("%d\n", magic(33));
  return 0;
}
