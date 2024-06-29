
create table employes (hours integer);
create table departments (hours integer);
insert into employes values (1), (2);
insert into departments values (3);


--t1
begin isolation level read committed ;
select sum(hours) from employes ;
-- sum 3


--t2
begin isolation level read committed ;
  insert into employes values (1);
  insert into departments values (1);
  COMMIT;


--t1
select sum(hours) from departments ;
-- sum 4


--więc pomimo tego że t2 nie naruszyło integralności to t1 tak to zglosi
-- odpowiedni poziom to repeatable reads
