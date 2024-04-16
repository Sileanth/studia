with
urgent_orders AS (
    SELECT orders.o_orderkey, orders.o_orderdate, orders.o_orderpriority
    FROM orders
    WHERE
        (o_orderpriority = '2-HIGH' OR o_orderpriority = '1-URGENT')
        AND EXTRACT('month' from o_orderdate) = 8
)
SELECT DISTINCT o_orderkey
FROM urgent_orders join lineitem on o_orderkey = l_orderkey
WHERE o_orderdate + INTERVAL '120 days' < l_shipdate