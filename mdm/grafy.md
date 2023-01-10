# Grafy definicje


## Lemat o uścisku dłoni
$$\sum_{v \in V} deg(v) = 2 |E| = 2m$$ 
gdzie n to liczba wierzchołków a m to liczba krawędzi.

## Izomorfizm grafów

$$ G_1 \simeq G_2 \iff \exist f : G_1 \to G_2$$
Będące bijękcją, oraz takie że:
$$ \{v_1, v_2\} \in E(G_1) \iff \{f(v_1), f(v_2)\} \in E[G_2]$$

## Rodzaje grafów

### Graf pusty/bezkrawędziowy
$E(G) = \empty$

### Graf pełny|/ Klika | $K_n$
$|E(K_n)| = {n \choose 2}$

### Dopełnienie grafu | $\bar{G}$
$$ |E(\bar{G})| = {n \choose 2} - | E(G)|$$
$$\bar{\bar{G}} = G$$


### Graf regularny (k - regularny)
$$ \forall v \in G: deg(v) = k$$

### Grafy dwudzielne 
Mozna podzielić wierzchołki na dwa zbiory A, B takie że A B sumują się do całego grafu. I nie ma krawędzi między wierzchołkami w tym samym podzbiorze

## Grafy spójne 
No da się dojść
