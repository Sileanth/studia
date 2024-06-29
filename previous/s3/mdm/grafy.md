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

### Grafy spójne 
No da się dojść istnieje droga

## Tw o grafach dwudzielnych 
Graf jest dwudzielny $\iff$ wszystkie cykle są parzystej długości

Graf jest dwudzielny $\iff$ wszystkie jego spójne składowe są dwudzielne

## Warunki drzewa
- T jest grafem spójnym bez cykli
- T nie ma cykli i ma n-1 krawędzi
- T jest spójny i ma n-1 krawędzi
- T jest spójny i każda krawędź jest mostem
- Dowolne 2 wierzchołki łączy dokładnie 1 droga
- T nie ma cykli, ale dodanie jakiejkolwiek krawędzi tworzy cykl

## Las
to graf którego spójne są drzewami, albo też graf bez cykli

## Drzewiasty lemat
każde drzewo o conajmniej 2 wierzchołkach ma co najmniej 2 lisci

## Drzewo rozpinające

## Tw Cayleya
Liczba drzew o n wierzchołkach wynosi $n^{n-2}$


## Droga eulera 
Taka droga która przechodzi każdą krawędź dokładnie raz


## Cykl eulera
to co wyżej tylk ocykl a nie droga


## Jeśli graf ma cykl euelorowski 
to wszystkie jego wierzchołki mają stopień parzysty


## Jeśli graf ma drogę eulerowską
to co najwyżej dwa wierzchołki mają stopień nieparzysty

## Graf eulerowski 
czyli graf który ma cykl eulera

## Graf póleuloerowski 
czyli taki która droge eulera

## Tw fajne
G jest eulerowsie $\iff$
1. Każdy jego wierzchołek ma stopień parzysty
2. G spójne

## Tw fajne 2
G jest półeulerowsie $\iff$
1. to co najwyżej dwa wierzchołki mają stopień nieparzysty
2. G spójne

## droga hamiltona 
droga przechodząca przez każdy wierzchołek dokładnie raz

## cykl hamiltona
to co wyżej tylko cykl


# Grafy planarne

## $K_5$ nie jest planarne

## $K_{3,3}$ nie jest planarne

## Tw Kuratowaskiogo
Graf jest planarny równoważne temu że nie zawiera podgrafu homeomorficznego z $K_5$ lub $K_{3,3}$

## Wzór Eulera
G - grał płaski spójny, n - wierzchołki, m - krawędzie, f - ściany
$$ n - m + f = 2$$

## Planarne można pkolorować 5 kolorami

## Skojarzenie $\equiv$ Zbiór kraœedzi o rozłącznych krawędziach $\equiv$ podgraf o maksymalnym stopnia wierzchołka 1

## Tw Halla
Istnieję możliwość małżeństw gdy każdy podzbiór k dziewczny zna co najmniej k chłopców

## Kolorowanie krawędzi grafu
$$\Chi'(G) \geq deg(G)$$

## Tw Viziniga
$$\Chi'(G) \leq deg(G) + 1$$

## Tw koniga 
Graf dwudzielny implukje $$ \Chi'(G) = deg(G)$$


##  Tw koniga-egrevage
W grafie dwudzielnym największe skojarzenie ma moc równą najmniejszemu pokryciu wierzchołkowemu

### Pokrycie wierzchołowe
Zbiór wierzchołków pokrywające wszystkei krawędzie


## Tw brooka
Keśli $$\chi(x) = d +1$$ to G jest kliką lub cyklem nieparzystej długości