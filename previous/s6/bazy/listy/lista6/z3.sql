create table plan (teacher integer, hours integer);
insert into plan values (1,0);


--t1
begin isolation level repeatable read ;
update plan set hours = hours + 100
where teacher = 1;
select hours into asum from plan where teacher = 1;


--t2 
begin isolation level repeatable read ;
update plan set hours = hours + 100
where teacher = 1;
select hours into asum from plan where teacher = 1;
--reczny if
commit;


--to wywala wtedy t1
--a w read commited oba dzialaja


-- naturalne rozwianie to dodanie constraint na liczbe godzin
