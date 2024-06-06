create table Bombiarze(id INT PRIMARY KEY, stopien INT);
create table Agenci(id INT PRIMARY KEY, bombiarz INT);


-- to query nie zwraca bombiarzy bez agenta
insert into agenci values (1, NULL);
insert into bombiarze values (1,1);



-- to query jest poprawne,
-- bo jak przynajmniej jeden agent ma nulla
-- bo IN zwraca true(jesli bombiarz jest monitorowany, albo null)
-- i potem not neguje, a not od nulla to to null(xd)
-- to moze obserwowac kazdego panoptykon
SELECT id FROM Bombiarze
WHERE id NOT IN (SELECT bombiarz FROM Agenci);


-- potencjalni bombiarze
-- najpierw filtrujemy agentow z nullami by not in 
-- zachowywal sie sensownie
SELECT id FROM Bombiarze
WHERE id NOT IN (SELECT bombiarz FROM Agenci WHERE bombiarz IS NOT NULL);



--testy
INSERT INTO Bombiarze (id, stopien)
SELECT generate_series(1, 1000000), (random() * 100)::INT;

--nullowi agenci
INSERT INTO Agenci (id, bombiarz)
SELECT 
    generate_series(1, 1000000), 
    CASE
        WHEN random() < 0.1 THEN NULL
        ELSE (random() * 1000000)::INT + 1
    END;

--nienullowi agenci
INSERT INTO Agenci (id, bombiarz)
SELECT 
    generate_series(1, 1000000), 
 (random() * 1000000)::INT + 1;


--speedup potencjalnych
EXPLAIN ANALYZE
SELECT b.id 
FROM Bombiarze b
LEFT JOIN Agenci a ON b.id = a.bombiarz
WHERE a.bombiarz IS NULL;


--speedup pewniaczkow
WITH NullCheck AS (
    SELECT 1 AS has_null
    FROM Agenci
    WHERE bombiarz IS NULL
    LIMIT 1
)

SELECT b.id
FROM Bombiarze b
LEFT JOIN Agenci a ON b.id = a.bombiarz
WHERE NOT EXISTS (SELECT 1 FROM NullCheck)
  AND a.bombiarz IS NULL;

select b.id FROM bombiarze b 
  WHERE not exists (select 1 from agenci where agenci.bombiarz = b.id);
 Hash Anti Join  (cost=30832.00..65042.43 rows=486133 width=4) (actual time=740.542..2212.117 rows=367715 loops=1)
   Hash Cond: (b.id = agenci.bombiarz)
   ->  Seq Scan on bombiarze b  (cost=0.00..14425.00 rows=1000000 width=4) (actual time=0.028..271.947 rows=1000000 loops=1)
   ->  Hash  (cost=14425.00..14425.00 rows=1000000 width=4) (actual time=739.679..739.681 rows=1000000 loops=1)
         Buckets: 262144  Batches: 8  Memory Usage: 6461kB
         ->  Seq Scan on agenci  (cost=0.00..14425.00 rows=1000000 width=4) (actual time=0.014..286.299 rows=1000000 loops=1)
 Planning Time: 0.433 ms
 Execution Time: 2245.550 ms
(8 rows)
