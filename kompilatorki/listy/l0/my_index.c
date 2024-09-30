unsigned long my_strlen(const char *s);
const char *my_index(const char *s, char v) {
  for (unsigned long i = 0; i < my_strlen(s); i++)
    if (s[i] == v)
      return &s[i];
  return 0;
}
