
CREATE TABLE company (
    id SERIAL PRIMARY KEY
);
CREATE TABLE oferty (
    id SERIAL PRIMARY KEY,
    company_id INT,
    FOREIGN KEY (company_id) REFERENCES company(id)
);

INSERT INTO company (id)
SELECT generate_series(1, 50000);

INSERT INTO oferty (company_id)
SELECT (random() * 49900 + 1)::int
FROM generate_series(1, 50000);


EXPLAIN ANALYZE
DELETE FROM company c WHERE c.id NOT IN (
SELECT company_id FROM oferty WHERE company_id IS NOT NULL
);

                                                       QUERY PLAN                                                        
-------------------------------------------------------------------------------------------------------------------------
 Delete on company c  (cost=848.52..1778.15 rows=0 width=0) (actual time=145.844..145.847 rows=0 loops=1)
   ->  Seq Scan on company c  (cost=848.52..1778.15 rows=28305 width=6) (actual time=60.000..100.954 rows=18347 loops=1)
         Filter: (NOT (hashed SubPlan 1))
         Rows Removed by Filter: 31653
         SubPlan 1
           ->  Seq Scan on oferty  (cost=0.00..723.72 rows=49921 width=4) (actual time=0.050..26.871 rows=50000 loops=1)
                 Filter: (company_id IS NOT NULL)
 Planning Time: 0.210 ms
 Trigger for constraint oferty_company_id_fkey: time=182019.763 calls=18347
 Execution Time: 182172.771 ms


create INDEX on oferty(company_id);

EXPLAIN ANALYZE
DELETE FROM company c WHERE c.id NOT IN (
SELECT company_id FROM oferty WHERE company_id IS NOT NULL
);
                                                 
-------------------------------------------------------------------------------------------------------------------------
 Delete on company c  (cost=847.00..1464.66 rows=0 width=0) (actual time=89.078..89.081 rows=0 loops=1)
   ->  Seq Scan on company c  (cost=847.00..1464.66 rows=15826 width=6) (actual time=89.074..89.076 rows=0 loops=1)
         Filter: (NOT (hashed SubPlan 1))
         Rows Removed by Filter: 31653
         SubPlan 1
           ->  Seq Scan on oferty  (cost=0.00..722.00 rows=50000 width=4) (actual time=0.434..24.666 rows=50000 loops=1)
                 Filter: (company_id IS NOT NULL)
 Planning Time: 12.023 ms
 Execution Time: 92.050 ms
(9 rows)
