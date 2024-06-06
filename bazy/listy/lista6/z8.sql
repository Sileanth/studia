
CREATE TABLE company (
    id SERIAL PRIMARY KEY
);
CREATE TABLE oferty (
    id SERIAL PRIMARY KEY,
    company_id INT,
    FOREIGN KEY (company_id) REFERENCES company(id)
);

INSERT INTO company (id)
SELECT generate_series(1, 500000);

INSERT INTO oferty (company_id)
SELECT (random() * 499900 + 1)::int
FROM generate_series(1, 500000);


EXPLAIN ANALYZE
DELETE FROM company c WHERE c.id NOT IN (
SELECT company_id FROM oferty WHERE company_id IS NOT NULL
);

create INDEX on oferty(company_id);

EXPLAIN ANALYZE
DELETE FROM company c WHERE c.id NOT IN (
SELECT company_id FROM oferty WHERE company_id IS NOT NULL
);
