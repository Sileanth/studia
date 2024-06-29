create table plan (teacher integer, course integer, hours integer);

-- musi byc serialized, bo inserty sa niewidzialne dla read commited oread repeatable reads

--t1
begin isolation level repeatable read ;
insert into plan values (1, 1, 150);  
SELECT SUM(hours) FROM plan WHERE teacher = 1;

--t2
begin isolation level repeatable read ;
insert into plan values (1, 1, 150);  
SELECT SUM(hours) FROM plan WHERE teacher = 1;


-- t1
--reczny IF
commit;

-- t2
-- reczny if 
commit;


--ten nauczyciel ma 300 godzin ups
