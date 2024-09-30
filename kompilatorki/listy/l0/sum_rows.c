// a - macierz n x n, b - wektor n
void sum_rows(double *restrict a, double *restrict b, long n) {
  for (long i = 0; i < n; i++) {
    b[i] = 0;
    for (long j = 0; j < n; j++)
      b[i] += a[i * n + j];
  }
}
