# Asymptotyczne definicje

$f(n) = O(g(n)) \iff \exists c, n_0 : \forall n > n_0: f(n) \leq c (g(n) )$

$f(n) = \Omega(g(n)) \iff \exists c > 0, n_0: \forall n > n_0:  f(n) \geq c( g(n))$

$f(n) = \Theta (g(n)) \iff \exists c,d : c g(n) \leq f(n) \leq d g(n)$ 

$f(n) \sim g(n) \iff \frac{f(n)}{g(n)} \lim_{n \to \infty} 1 $

$f(n) = o(g(n)) \iff \frac{f(n)}{g(n)} \lim_{n \to \infty} 0 $

$f(n) \prec g(n) \iff f = o(g)$
Częściowy porządek

# Liczby Fibbonaciego (dupa)
## Definicja
$F_1 = F_2 = 1, F_{n+1} = F_n + F_{n-1}$
## Własności
### a)
$F_2 + F_4 + \ldots + F_{2n} = F_{2n+1} -1$
### b)
$F_{n+m} = F_n F{m+1} + F_{n-1}F_{m}$dupa
### c)
$F_n = \frac1{\sqrt{5}} ( (\frac{1 + \sqrt{5}2})^n + (\frac{1 - \sqrt5}2)^n)$


# NWD oraz Mod (dupa)

## Definicja
$$ NWD (a, b) = max \{ d \in \mathbb{Z} : d | a \land d | b \}$$
$$ NWW (a, b) = min \{ c \in [\mathbb{N} \cup \{ 0\} ] : a | c \land b | c \}$$
$$ NWD (a, 0) = 0$$
$$ NWD (a, b) = NWD (a-b, b) = NWD(a -cb, b)$$

```python
def nwd(a, b):
    while b > 0:
        a, b = (b, a MOD b)
    return a
```

## Rozszerzony euklides
Oprócz NWD(a, b) znajduje także współczynniki $x,y \in \mathbb{Z}$, takie że :
$xa + yb = NWD(a,b)$

## Odwrotności w $\mathbb{Z}_n$

$$a \in \mathbb{Z}_n, x = a^{-1} \iff a *_n x = 1 \mod n$$
$$ a +_n b = (a + b) \mod n$$
$$ x \mod n = x - \left\lfloor \frac{x}{n} \right\rfloor * x $$
$$ a \cdot_n b = (a \cdot b) \mod n$$


$\mathbb{Z}$ jest grupa z $+_n$, elementem neuatrlnym jest 0$

### Fakt
Jeśli $NWD (a, n) > 1$ to nie istnieje odwrotność.

### Jak policzyć $a^{-1}$ 
Stosujemy rozszerzony algorytm euklidesa dla a i n, wtedy znajdujemy x, y takie że $$xa + yn = 1 \implies xa \equiv 1 \mod n$$
$$ a^{-1} \mod n = x \mod n$$
