CREATE VIEW DERIVATIVES
AS SELECT
          8 as IDXID,
          REGIONS.ISOID as ISOID,
          'VALUES'.YEAR as YEAR,
          'VALUES'.VALUE / CAST(REGIONS.AREA as REAL) AS VALUE,
          'VALUES'.SRCID as SRCID
FROM 'VALUES',REGIONS
WHERE 'VALUES'.IDXID=0 AND 'VALUES'.ISOID=REGIONS.ISOID