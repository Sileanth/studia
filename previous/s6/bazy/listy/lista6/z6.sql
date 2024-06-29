create table employees (renumaration float, no_of_articles integer);
insert into employees values (2.5, 3), (3,3), (2.1,2), (3.14, 2);

-- read commtied sie popsuje(repeatable readbedzie dobre)


--t1
begin isolation level read committed ;
update employees set no_of_articles = no_of_articles + 2;
insert into employees values (11.1, 3);

--t2
begin isolation level read committed ;
  update employees 
set renumaration = 1.10 * renumaration
where no_of_articles BETWEEN 3 AND 4;


--t1 
commit;

--t2
--nikt nie dostal podwyzki
commit;


-- w reapetable reads, updaty spowoduja rollback, insert zignoruje(tak jakby podwyzka byla zanim ktos sie dostanie)
