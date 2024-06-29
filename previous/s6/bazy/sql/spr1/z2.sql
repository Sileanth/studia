SELECT  lineitem.l_orderkey
FROM lineitem
GROUP BY l_orderkey
Having COUNT(DISTINCT l_shipmode) = 1 AND COUNT(l_linenumber) > 1
ORDER BY l_orderkey

