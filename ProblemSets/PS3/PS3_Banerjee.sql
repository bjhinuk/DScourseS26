-- ============================================

-- Problem Set 3 - SQL Script

-- Jhinuk Banerjee

-- ============================================

-- (a) Read in the Florida insurance data CSV file

-- First, we need to create a table to hold the data

.mode csv

.import FL_insurance_sample.csv insurance_data

-- (b) Print out the first 10 rows of the data set

-- Display the first 10 rows to see what the data looks like

SELECT * FROM insurance_data LIMIT 10;

-- (c) List which counties are in the sample

-- Get unique values of the county variable

SELECT DISTINCT county 

FROM insurance_data 

ORDER BY county;

-- (d) Compute the average property appreciation from 2011 to 2012

-- Calculate the mean of (tiv_2012 - tiv_2011)

SELECT AVG(tiv_2012 - tiv_2011) AS avg_appreciation

FROM insurance_data;

-- (e) Create a frequency table of the construction variable

-- See what fraction of buildings are made of different materials

SELECT 

    construction,

    COUNT(*) AS frequency,

    ROUND(COUNT(*) * 100.0 / (SELECT COUNT(*) FROM insurance_data), 2) AS percentage

FROM insurance_data

GROUP BY construction

ORDER BY frequency DESC;

