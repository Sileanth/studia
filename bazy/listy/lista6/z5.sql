create table enrol (student integer, course integer, slot integer);


-- musi byc serializable
--t1 
begin isolation level repeatable read ;
  select count(*) into a1 from enrol where student = 1 and course = 1;


--t2 
 begin isolation level repeatable read ;
  select count(*) into a1 from enrol where student = 1 and course = 1;
--reczny if 
insert into enrol values (1,1,2);
commit;

--t1
--reczny if 
insert into enrol values (1,1,1);
commit;



--zapisal sie na 2 terminy
