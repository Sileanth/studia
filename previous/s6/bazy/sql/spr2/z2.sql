WITH
    cust_nation_sum AS (
        SELECT o_custkey, SUM(o_totalprice) as price_sum, c_nationkey
        FROM customer join orders on c_custkey = o_custkey
        WHERE extract(YEAR FROM o_orderdate) = 1997
        GROUP BY o_custkey, c_nationkey
    ),
    nation_best AS (
        SELECT c_nationkey, MAX(price_sum) as best
        FROM cust_nation_sum
        GROUP BY c_nationkey
    ),
    nation_best_cust AS (
        SELECT a.c_nationkey, a.o_custkey, price_sum
        FROM cust_nation_sum a JOIN nation_best b ON a.c_nationkey = b.c_nationkey AND price_sum = best
    )
SELECT n_name, price_sum, c_name from
    (nation LEFT JOIN nation_best_cust on c_nationkey = nation.n_nationkey)
    JOIN customer ON o_custkey = c_custkey