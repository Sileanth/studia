with asia_n_keys AS (
    SELECT n_nationkey, n_name
    FROM nation JOIN region on n_regionkey = r_regionkey
    WHERE r_name = 'ASIA'
),
asian_sup AS (
    SELECT s_name, s_suppkey
    FROM asia_n_keys JOIN supplier ON s_nationkey = n_nationkey
),
goodparts AS (
    SELECT part.p_partkey
    FROM part
    WHERE p_type LIKE '%BRUSHED BRASS' AND p_size = 50
),
good_sup_parts AS (
    SELECT ps_suppkey
    FROM goodparts JOIN partsupp on p_partkey = ps_partkey
)
SELECT DISTINCT s_name
FROM good_sup_parts JOIN asian_sup on s_suppkey = ps_suppkey