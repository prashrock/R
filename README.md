R
=
- This contains some sample R code for the following three purposes:
  - Visualizing mathematical functions
  - Trying out data-mining algorithms
  - Trying out machine learning algorithms

###math_visualize.R###
Plot sine/cosine/log/sigmoid functions

###mysql_per_column_analysis.R:###
Executes the below query on each column given a table. Can be used to do per-column aggregation for initial data analysis:
```SQL
SELECT col_name, COUNT(*) AS cnt
FROM table_name
GROUP BY col_name
ORDER BY cnt DESC
LIMIT 20
```
