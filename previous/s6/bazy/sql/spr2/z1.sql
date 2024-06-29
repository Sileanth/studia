WITH
    eu_clients AS (
        SELECT c_custkey, c_name
        FROM customer
        WHERE exists(
            SELECT *
            FROM nation
            WHERE
                n_nationkey = customer.c_nationkey
            AND n_regionkey IN (
                SELECT r_regionkey
                FROM region
                WHERE r_name = 'EUROPE                   '
            )
        )

    )
SELECT c_name, SUM(o_totalprice) as sum_price
FROM eu_clients join orders on c_custkey = o_custkey
WHERE extract('YEAR' FROM o_orderdate) = 1997
GROUP BY c_name
HAVING SUM(o_totalprice) > 500000
ORDER BY c_name DESC
