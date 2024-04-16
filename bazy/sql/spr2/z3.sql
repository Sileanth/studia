-- noinspection SqlRedundantCodeInCoalesceForFile
WITHjumbo_partsAS(
  SELECT
    p_name,
    part.p_partkey
  FROM
    part
  WHERE
    p_container = 'JUMBO PKG'
),
reg_supAS(
  SELECT
    s_suppkey,
    r_name
  FROM
    (
      supplier
    JOIN nation
      ON s_nationkey = n_nationkey
    )
  JOIN region r
    ON n_regionkey = r_regionkey
  WHERE
    r_name = 'MIDDLE EAST'
),
gpsAS(
  SELECT
    p_partkey,
    p_name,
    SUM(COALESCE(ps_availqty, 0)) AS psum
  FROM
    (
      jumbo_parts
    LEFT JOIN (
      partsupp
    JOIN reg_sup
      ON ps_suppkey = s_suppkey
    )
      ON p_partkey = ps_partkey
    )
  GROUP BY
    p_partkey,
    p_name
)SELECT
  p_name,
  psum
FROM
  gps
