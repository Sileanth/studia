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

# Liczby pierwsze
## definicja
Liczba pierwsza to taka która dzieli się tylko przez siebie i 1.

## Rozkład liczby 
Każda liczba $n \in \mathbb{N}$ ma rozkład ma jednoznaczny rozkład na liczby pierwsze, z dokładnością do kolejności czynników
$$ n = p_1 \cdots p_2 \ldots p_k$$

## Tw. Euklidesa liczb pierwszych jest nieskończenie wiele
    prosty dowodzik

## $\Pi (n):$ ilość liczb pierwszych na przedziale $[1, n]$
$$ \Pi(n) \sim \frac{n}{\log n}$$

## Chińskie twierdzenie o resztach
$$ m = m_1 \ldots m_k \land( m_i \perp m_j \iff i \neq j)$$ 
Układ kongruencji $(a_i \in \mathbb{Z}_{m_i})$
$$ x \equiv a_1 \mod m_1$$
$$ x \equiv a_2 \mod m_2$$
$$\ldots$$
$$x \equiv a_k \mod m_k$$
Ma dokładnie jedno rozwiązanie $x \in \mathbb{z}_m$

## Funkcja Eulera $\phi (n)$
### Definicja
$$ \phi (n) = \{ x \in \mathbb{Z}_n : x \perp n\}$$

### Fakt
$$\mathbb{Z}_n^* = \{ a \in \mathbb{Z}_n : \exists a^{-1} \}$$
Jest to grupa oraz $\phi(n) = |\mathbb{Z}_n^*|$

### Twierdzenie
$$n = p_1^{k_1} \cdots p_2^{k_2} \ldots p_s^{k_s} \implies \phi(n) = n (1 - \frac1{p_1}) \ldots (1 - \frac1{p+s})$$

### Twierdzenie Eulera 
$$a \perp n \implies a^{\phi(n)} \equiv 1 \mod n$$

####  Małe twierdzenie Fermata(szczególny przypadek)
$$a^{p-1} \equiv 1 \mod p$$

### Twierdzonko
$$\forall a \perp n: \exists x \in \mathbb{N} :  a^x \equiv 1 \mod n$$

# Kombinatoryka
## Zasada szufladkowa Dirichleta
$(kn + 1)$ kulek $\implies $ istnieje szufladka z $k+1$ kulkami.

## Basic wzorki na ciągi
k - ilość elementów, n - liczba możliwych wartości
### Ciągi z powtórzeniami 

$$n^k$$
### Ciągi bez powtórzeń
$$ n!$$
### Możliwe podzbiory
$$ { n \choose k } = \frac{n!}{(n-k)! k!}$$

### Dwumian netwona
$$ {n \choose k} = {n -1 \choose k -1} + { n - 1 \choose k}$$

## Wzór dwumienny Newtona
$$(a+b)^n = {n \choose 0} a^n + {n \choose 1} a^{n-1} b + \ldots + {n \choose n} b^n$$

## Zasada włączeń i wyłączeń
$$ |A \cup B| = |A| + |B| - |A \cap B|$$
$$ | \bigcup_{i=1}^k A_k| = \sum_{i_1 \ldots i_s \leq k+1 } (-1)^{S+1}|A_{i-1} \cap \ldots \cap A_{i_S}|$$

# Grupy

## Tw Lagrange
Moc grupy to liczba warst razy moc warstwy
$$|G| = [G : X] * |H| $$

## Orbita
$$|O_x| =\{ y : \exists g \in G , y = g(x)  \}$$

## Stabilizator

$$G_x = \{  g \in G : g(x) = x  \}$$

## Lemacik
$$|G| = |G_x| * |O_x|$$

## Lemat burnsida
$$|orbit| = \frac{1}{|G|} \sum_{x \in X} |G_x| = \frac1{G} \sum_{g \in G} |Fix(g)|$$
gdzie 
$$Fix(g) = \{ x \in X : g(x) = x \}$$
 

