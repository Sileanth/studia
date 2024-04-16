select *
from customer
where c_nationkey = 24 AND c_acctbal > 9000
order by c_name DESC