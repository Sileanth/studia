SELECT *
FROM (SELECT c_custkey FROM customer) as ccc
EXCEPT (SELECT DISTINCT o_custkey as c_custkey from orders)
