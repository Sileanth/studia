# Konstrukcja kompilatorów

## Lista zadań nr 0

*Na zajęcia 2 października 2024*

**UWAGA!** Poniższe zadania należy rozwiązać używając kompilatora GCC w
wersji 6 (lub wyższej) generującego kod dla procesorów x86-64. W linii
poleceń kompilatora *musisz* dodać następujące opcje: `-march=nehalem
-fomit-frame-pointer -Os`!

**Wskazówka:** Przed przystąpieniem do rozwiązywania zadań należy zapoznać
się z rozdziałem piątym trzeciego wydania podręcznika "Computer Systems:
A Programmer's Perspective".

### Notes

| 64-bit Register | 32-bit Register | 16-bit Register | 8-bit Register | Purpose       |
|-----------------|-----------------|-----------------|----------------|---------------|
| %rax            | %eax            | %ax             | %al            | Return value  |
| %rbx            | %ebx            | %bx             | %bl            | Callee saved  |
| %rcx            | %ecx            | %cx             | %cl            | 4th argument  |
| %rdx            | %edx            | %dx             | %dl            | 3rd argument  |
| %rsi            | %esi            | %si             | %sil           | 2nd argument  |
| %rdi            | %edi            | %di             | %dil           | 1st argument  |
| %rbp            | %ebp            | %bp             | %bpl           | Callee saved  |
| %rsp            | %esp            | %sp             | %spl           | Stack pointer |
| %r8             | %r8d            | %r8w            | %r8b           | 5th argument  |
| %r9             | %r9d            | %r9w            | %r9b           | 6th argument  |
| %r10            | %r10d           | %r10w           | %r10b          | Caller saved  |
| %r11            | %r11d           | %r11w           | %r11b          | Caller saved  |
| %r12            | %r12d           | %r12w           | %r12b          | Callee saved  |
| %r13            | %r13d           | %r13w           | %r13b          | Callee saved  |
| %r14            | %r14d           | %r14w           | %r14b          | Callee saved  |
| %r15            | %r15d           | %r15w           | %r15b          | Callee saved  |

### Zadanie 1.

Skompiluj poniższą procedurę, a następnie na podstawie
wyprodukowanego kodu w asemblerze zapisz na tablicy zoptymalizowany
program w języku C. Jakie właściwości programu wykrył kompilator i
wykorzystał je w trakcie optymalizacji?

```c
void set_row(long *a, long *b, long i, long n) {
  for (long j = 0; j < n; j++)
    a[n * i + j] = b[j];
}
```

| variable | long *a | long *b | long i | long n | long j | long j |
|----------|---------|---------|--------|--------|--------|--------|
| register | %rdi    | %rsi    | %rdx   | $rcx   | %rax   | %      |

*gcc -march=nehalem -fomit-frame-pointer -Os -c set_row.c*
*objdump -d set_row.o*

```asm
 0:	48 0f af d1          	imul   %rcx,%rdx
 4:	31 c0                	xor    %eax,%eax
 6:	48 8d 14 d7          	lea    (%rdi,%rdx,8),%rdx
 a:	48 39 c8             	cmp    %rcx,%rax
 d:	7d 0d                	jge    1c <set_row+0x1c>
 f:	48 8b 3c c6          	mov    (%rsi,%rax,8),%rdi
13:	48 89 3c c2          	mov    %rdi,(%rdx,%rax,8)
17:	48 ff c0             	inc    %rax
1a:	eb ee                	jmp    a <set_row+0xa>
1c:	c3                   	ret
```

```c
void set_row(long *a, long *b, long i, long n) {
  i *= n;
  long *ai = &a[i];
  for(int j = 0; j < n; j++) {
    long temp = b[j];
    ai[j] = temp;
  }
}
```

Kompilator zauważył że wartości i oraz n są stałe więc wystarczy tylko raz policzyć (i * n)

### Zadanie 2.

Skompiluj poniższą procedurę i przeanalizuj wygenerowany
kod w asemblerze. Jakie założenie poczynił kompilator na podstawie
oznaczenia wskaźników `a` i `b` słowem kluczowym `restrict`?

```c
// a - macierz n x n, b - wektor n 
void sum_rows(double *restrict a, double *restrict b, long n) {
  for(long i = 0; i < n; i++) {
    b[i] = 0;
    for (long j = 0; j < n; j++)
      b[i] += a[i * n + j];
  }
}
```

*gcc -march=nehalem -fomit-frame-pointer -Os -c sum_rows.c*
*objdump -d sum_rows.o*

| var | double * a | double* b | long n | long i | long j |
|-----|------------|-----------|--------|--------|--------|
| reg | %rdi       | %rsi      | %rdx   | %rax   | %r8d   |

```asm
 0:	45 31 c0             	xor    %r8d,%r8d
 3:	31 c0                	xor    %eax,%eax
 5:	48 39 d0             	cmp    %rdx,%rax
 8:	7d 24                	jge    2e <sum_rows+0x2e>
 a:	4e 8d 0c c7          	lea    (%rdi,%r8,8),%r9
 e:	31 c9                	xor    %ecx,%ecx
10:	0f 57 c0             	xorps  %xmm0,%xmm0
13:	f2 41 0f 58 04 c9    	addsd  (%r9,%rcx,8),%xmm0
19:	48 ff c1             	inc    %rcx
1c:	48 39 ca             	cmp    %rcx,%rdx
1f:	75 f2                	jne    13 <sum_rows+0x13>
21:	f2 0f 11 04 c6       	movsd  %xmm0,(%rsi,%rax,8)
26:	49 01 d0             	add    %rdx,%r8
29:	48 ff c0             	inc    %rax
2c:	eb d7                	jmp    5 <sum_rows+0x5>
2e:	c3                   	ret
```

**Wskazówka:** Rozważ liczbę generowanych dostępów do poszczególnych
elementów tablicy `b`.

Odczytujemy b tylko n razy, bo nie trzeba tablicy aktualizować na bieżąco,
bo z tablicy b nie czytamy zaś tablica a nie pokrywa się z tablicą b, więc na nią nie mają wpływu aktualizacje b

Dzięki temu ograniczemy znacząco liczbę dostępów do pamięci, co jest wolne

### Zadanie 3.

Z użyciem składni `__attribute__` programista może
poinformować kompilator o szczególnych własnościach procedur, zmiennych,
itd. W poniższym przypadku oznaczyliśmy procedurę `my_strlen`, która
zachowuje się identycznie jak `strlen` z biblioteki standardowej, jako
*funkcję czystą*. Jakiej optymalizacji nie byłby w stanie przeprowadzić
kompilator bez tej informacji?

```c
__attribute__((pure)) unsigned long my_strlen(const char *s); 
const char *my_index(const char *s, char v) {
  for (unsigned long i = 0; i < my_strlen(s); i++)
    if (s[i] == v)
      return &s[i]; return 0;
}
```

*pure version*
```asm
 0:	31 c0                	xor    %eax,%eax
 2:	0f be 0f             	movsbl (%rdi),%ecx
 5:	39 f1                	cmp    %esi,%ecx
 7:	48 0f 44 c7          	cmove  %rdi,%rax
 b:	84 c9                	test   %cl,%cl
 d:	74 05                	je     14 <strrchr+0x14>
 f:	48 ff c7             	inc    %rdi
12:	eb ee                	jmp    2 <strrchr+0x2>
14:	c3                   	ret
```

*impure version*
```asm
 0:	41 54                	push   %r12
 2:	41 89 f4             	mov    %esi,%r12d
 5:	55                   	push   %rbp
 6:	31 ed                	xor    %ebp,%ebp
 8:	53                   	push   %rbx
 9:	48 89 fb             	mov    %rdi,%rbx
 c:	48 89 df             	mov    %rbx,%rdi
 f:	e8 00 00 00 00       	call   14 <my_index+0x14>
14:	48 39 c5             	cmp    %rax,%rbp
17:	73 11                	jae    2a <my_index+0x2a>
19:	44 38 24 2b          	cmp    %r12b,(%rbx,%rbp,1)
1d:	75 06                	jne    25 <my_index+0x25>
1f:	48 8d 04 2b          	lea    (%rbx,%rbp,1),%rax
23:	eb 07                	jmp    2c <my_index+0x2c>
25:	48 ff c5             	inc    %rbp
28:	eb e2                	jmp    c <my_index+0xc>
2a:	31 c0                	xor    %eax,%eax
2c:	5b                   	pop    %rbx
2d:	5d                   	pop    %rbp
2e:	41 5c                	pop    %r12
30:	c3                   	ret
```

**Wskazówka:** Ile razy procedura `my_index` wywoła funkcję `my_strlen`?

Jako że funkcja my_strlen jest funkcją czystą i nie modyfikujemy tablicy s to wystarczy tylko raz zawołać funkcje
my_strlen.

### Zadanie 4.

Poniżej podano kod procedury `strrchr` z biblioteki
standardowej. Przetłumacz go na *kod trójkowy* (ang. *three-address
code*), podziel na *bloki podstawowe* (ang. *basic block*), a następnie
narysuj *graf przepływu sterowania* (ang. *control flow graph*).

```c
char *strrchr(const char *cp, int ch) {
  char *p = (char *)cp;
  for (char *save = 0;; ++p) {
      if (*p == ch)
       save = p;
      if (*p == '\0')
       return save;
  }
}
```

*gcc -march=nehalem -fomit-frame-pointer -Os -fdump-tree-all -o z4/strrchr.o -c strrchr.c*
*cat ./z4/*.cfg*

```
;; Function strrchr (strrchr, funcdef_no=0, decl_uid=1715, cgraph_uid=1, symbol_order=0)

Merging blocks 6 and 8
;; 2 loops found
;;
;; Loop 0
;;  header 0, latch 1
;;  depth 0, outer -1
;;  nodes: 0 1 2 3 4 5 6 7
;;
;; Loop 1
;;  header 3, latch 7
;;  depth 1, outer 0
;;  nodes: 3 7 5 4
;; 2 succs { 3 }
;; 3 succs { 4 5 }
;; 4 succs { 5 }
;; 5 succs { 6 7 }
;; 6 succs { 1 }
;; 7 succs { 3 }
__attribute__((nothrow, leaf, pure))
__attribute__((nonnull))
char * strrchr (const char * cp, int ch)
{
  char * save;
  char * p;
  char * D.2889;

  <bb 2> :
  p = cp;
  save = 0B;

  <bb 3> :
  _1 = *p;
  _2 = (int) _1;
  if (ch == _2)
    goto <bb 4>; [INV]
  else
    goto <bb 5>; [INV]

  <bb 4> :
  save = p;

  <bb 5> :
  _3 = *p;
  if (_3 == 0)
    goto <bb 6>; [INV]
  else
    goto <bb 7>; [INV]

  <bb 6> :
  D.2889 = save;
  // predicted unlikely by early return (on trees) predictor.
  return D.2889;

  <bb 7> :
  p = p + 1;
  goto <bb 3>; [INV]

}
```

Zapisz powyższą procedurę do pliku `strrchr.c`. Następnie skompiluj go z
opcją `-fdump-tree-all` i porównaj swoje rozwiązanie z plikiem o sufiksie
`cfg` (np. `strrchr.c.011t.cfg`).

[Previous content remains unchanged]

### Zadanie 5.

Skompiluj poniższy kod z opcją `-O0` i uruchom otrzymany program. Następnie ponownie skompiluj go z opcją `-Os` i przeanalizuj otrzymany kod w asemblerze. Wytłumacz dlaczego kompilator aż tak drastycznie skrócił kod.

```c
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
```

*gcc -march=nehalem -fomit-frame-pointer -O0 -c magic_main_naive.c*
*objdump -d magic_main_naive.c*
```asm
0000000000000000 <magic>:
   0:	89 7c 24 ec          	mov    %edi,-0x14(%rsp)
   4:	c7 44 24 f8 00 00 00 	movl   $0x0,-0x8(%rsp)
   b:	00 
   c:	c7 44 24 fc 01 00 00 	movl   $0x1,-0x4(%rsp)
  13:	00 
  14:	eb 33                	jmp    49 <magic+0x49>
  16:	8b 44 24 fc          	mov    -0x4(%rsp),%eax
  1a:	33 44 24 ec          	xor    -0x14(%rsp),%eax
  1e:	01 44 24 f8          	add    %eax,-0x8(%rsp)
  22:	8b 54 24 ec          	mov    -0x14(%rsp),%edx
  26:	89 d0                	mov    %edx,%eax
  28:	01 c0                	add    %eax,%eax
  2a:	01 d0                	add    %edx,%eax
  2c:	c1 e0 02             	shl    $0x2,%eax
  2f:	01 d0                	add    %edx,%eax
  31:	89 44 24 ec          	mov    %eax,-0x14(%rsp)
  35:	8b 44 24 fc          	mov    -0x4(%rsp),%eax
  39:	89 c2                	mov    %eax,%edx
  3b:	c1 ea 1f             	shr    $0x1f,%edx
  3e:	01 d0                	add    %edx,%eax
  40:	d1 f8                	sar    $1,%eax
  42:	83 c0 01             	add    $0x1,%eax
  45:	01 44 24 fc          	add    %eax,-0x4(%rsp)
  49:	83 7c 24 fc 00       	cmpl   $0x0,-0x4(%rsp)
  4e:	7f c6                	jg     16 <magic+0x16>
  50:	8b 44 24 f8          	mov    -0x8(%rsp),%eax
  54:	6b c0 2a             	imul   $0x2a,%eax,%eax
  57:	c3                   	ret

0000000000000058 <main>:
  58:	48 83 ec 08          	sub    $0x8,%rsp
  5c:	bf 21 00 00 00       	mov    $0x21,%edi
  61:	e8 9a ff ff ff       	call   0 <magic>
  66:	89 c6                	mov    %eax,%esi
  68:	48 8d 05 00 00 00 00 	lea    0x0(%rip),%rax        # 6f <main+0x17>
  6f:	48 89 c7             	mov    %rax,%rdi
  72:	b8 00 00 00 00       	mov    $0x0,%eax
  77:	e8 00 00 00 00       	call   7c <main+0x24>
  7c:	b8 00 00 00 00       	mov    $0x0,%eax
  81:	48 83 c4 08          	add    $0x8,%rsp
  85:	c3   
```

*gcc -march=nehalem -fomit-frame-pointer -Os -c magic_main_opt.c*
*objdump -d magic_main_opt.o*
```asm
     0:	eb fe                	jmp    0 <main>
```

**Wskazówka:** Znajdź co standard języka C definiuje dla przepełnienia podczas dodawania liczb całkowitych ze znakiem. Co dzięki temu może założyć kompilator o dodawaniu dwóch liczb dodatnich?



Overflow intów ze znakiem to undefined behavior, więc może się cokolwiek zdarzyć w tym kodzie
W szczególności więc może założyć że w poprawnych przypadkach:
dodanie dwóch dodatnich liczb zawsze daje liczbę dodatnią.
Czyli (x > 0) będzie zawsze prawdziwe czyli program się zapętli.

