
CREATE TABLE company (
    id SERIAL PRIMARY KEY
);




CREATE TABLE offer (
    id SERIAL PRIMARY KEY,
    company_id INTEGER NOT NULL,
    publish_date DATE NOT NULL,
    CONSTRAINT fk_company
        FOREIGN KEY (company_id) 
        REFERENCES company (id)
);


INSERT INTO company (id)
SELECT generate_series(1, 300000);



INSERT INTO offer (company_id, publish_date)
SELECT
  (random() * 299990 + 1)::int AS company_id, 
  (date '2023-01-01' + interval '1 day' * floor(random() * 364))::date AS publish_date 
FROM generate_series(1, 300000);


EXPLAIN ANALYZE
select * from offer 
where company_id >= 100 and company_id <= 200;
                                                     QUERY PLAN                                                      
---------------------------------------------------------------------------------------------------------------------
 Gather  (cost=1000.00..5279.96 rows=109 width=12) (actual time=0.865..58.015 rows=130 loops=1)
   Workers Planned: 1
   Workers Launched: 1
   ->  Parallel Seq Scan on offer  (cost=0.00..4269.06 rows=64 width=12) (actual time=0.591..45.068 rows=65 loops=2)
         Filter: ((company_id >= 100) AND (company_id <= 200))
         Rows Removed by Filter: 149935
 Planning Time: 0.448 ms
 Execution Time: 58.081 ms
(8 rows)

EXPLAIN ANALYZE
select * from offer 
where company_id = 100;
                                                     QUERY PLAN                                                     
--------------------------------------------------------------------------------------------------------------------
 Gather  (cost=1000.00..4828.08 rows=2 width=12) (actual time=54.951..61.001 rows=1 loops=1)
   Workers Planned: 1
   Workers Launched: 1
   ->  Parallel Seq Scan on offer  (cost=0.00..3827.88 rows=1 width=12) (actual time=36.560..46.531 rows=0 loops=2)
         Filter: (company_id = 100)
         Rows Removed by Filter: 150000
 Planning Time: 0.158 ms
 Execution Time: 61.046 ms
(8 rows)


EXPLAIN ANALYZE
SELECT *
FROM offer
WHERE publish_date BETWEEN '2023-09-01' AND '2023-09-10';
                                                QUERY PLAN                                                
----------------------------------------------------------------------------------------------------------
 Seq Scan on offer  (cost=0.00..6122.00 rows=8622 width=12) (actual time=0.025..95.071 rows=8107 loops=1)
   Filter: ((publish_date >= '2023-09-01'::date) AND (publish_date <= '2023-09-10'::date))
   Rows Removed by Filter: 291893
 Planning Time: 0.863 ms
 Execution Time: 96.106 ms
(5 rows)


create INDEX idx_pub_date on offer(publish_date);


EXPLAIN ANALYZE
select * from offer 
where company_id >= 100 and company_id <= 200;

                                                     QUERY PLAN                                                      
---------------------------------------------------------------------------------------------------------------------
 Gather  (cost=1000.00..5279.96 rows=109 width=12) (actual time=0.742..55.346 rows=130 loops=1)
   Workers Planned: 1
   Workers Launched: 1
   ->  Parallel Seq Scan on offer  (cost=0.00..4269.06 rows=64 width=12) (actual time=0.651..43.085 rows=65 loops=2)
         Filter: ((company_id >= 100) AND (company_id <= 200))
         Rows Removed by Filter: 149935
 Planning Time: 0.411 ms
 Execution Time: 55.436 ms
(8 rows)





                                                     QUERY PLAN                                                     
--------------------------------------------------------------------------------------------------------------------
 Gather  (cost=1000.00..4828.08 rows=2 width=12) (actual time=23.281..49.363 rows=1 loops=1)
   Workers Planned: 1
   Workers Launched: 1
   ->  Parallel Seq Scan on offer  (cost=0.00..3827.88 rows=1 width=12) (actual time=27.164..37.125 rows=0 loops=2)
         Filter: (company_id = 100)
         Rows Removed by Filter: 150000
 Planning Time: 0.130 ms
 Execution Time: 49.402 ms
(8 rows)


EXPLAIN ANALYZE
SELECT *
FROM offer
WHERE publish_date BETWEEN '2023-09-01' AND '2023-09-10';


                                                              QUERY PLAN                                                               
---------------------------------------------------------------------------------------------------------------------------------------
 Bitmap Heap Scan on offer  (cost=120.80..1872.13 rows=8622 width=12) (actual time=2.779..10.743 rows=8107 loops=1)
   Recheck Cond: ((publish_date >= '2023-09-01'::date) AND (publish_date <= '2023-09-10'::date))
   Heap Blocks: exact=1616
   ->  Bitmap Index Scan on offer_publish_date_idx  (cost=0.00..118.64 rows=8622 width=0) (actual time=1.938..1.939 rows=8107 loops=1)
         Index Cond: ((publish_date >= '2023-09-01'::date) AND (publish_date <= '2023-09-10'::date))
 Planning Time: 0.626 ms
 Execution Time: 11.891 ms
(7 rows)


CLUSTER offer USING idx_pub_date;
EXPLAIN ANALYZE
SELECT *
FROM offer
WHERE publish_date BETWEEN '2023-09-01' AND '2023-09-10';

                                                         QUERY PLAN                                                          
-----------------------------------------------------------------------------------------------------------------------------
 Bitmap Heap Scan on offer  (cost=121.19..1873.09 rows=8660 width=12) (actual time=0.795..3.723 rows=8279 loops=1)
   Recheck Cond: ((publish_date >= '2023-09-01'::date) AND (publish_date <= '2023-09-10'::date))
   Heap Blocks: exact=45
   ->  Bitmap Index Scan on idx_pub_date  (cost=0.00..119.02 rows=8660 width=0) (actual time=0.751..0.752 rows=8279 loops=1)
         Index Cond: ((publish_date >= '2023-09-01'::date) AND (publish_date <= '2023-09-10'::date))
 Planning Time: 0.557 ms
 Execution Time: 4.901 ms
(7 rows)
