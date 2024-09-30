void set_row(long *a, long *b, long i, long n) {
  for (long j = 0; j < n; j++)
    a[n * i + j] = b[j];
}
