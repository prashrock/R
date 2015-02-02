install.packages("RMySQL")
library(RMySQL)   # this will also load DBI 

#Global Variables
dbname <- 'my_personal_db'

#Function to create a MySQL Connection
mysql_connect <- function(database_name) {
  con <- dbConnect(dbDriver("MySQL"), dbname=database_name, host="localhost", username="abc", password="xyz")
  return(con)
}

#Blocking function to execute a query and get response (response is a data frame)
mysql_query <- function(dbname, query) {
  con <- mysql_connect(dbname)
  result <- dbGetQuery(con, statement = query)
  dbDisconnect(con)
  return(result)
}

#Given a table, execute a query on each columns and write the results to the console and to a file (table_name.txt)
query_each_col_in_a_table <- function(dbname, table_name) {
  #First get the column list for the given table
  name_query <- sprintf("SELECT column_name FROM INFORMATION_SCHEMA.COLUMNS WHERE table_name='%s';", table_name)
  print(paste0("Columns in table - ", table_name, ", given below:", collapse = NULL))
  columns <- as.vector(mysql_query(dbname, name_query))

  #Second, create a vector containing all the queries to execute (1 query per column)
  #This query can be anything. Below code uses "group by" as an example
  col_query <- vector("character", length=nrow(columns))
  for(i in 1:nrow(columns)) {
    col_query[i] <- sprintf("SELECT %s, COUNT(*) AS cnt FROM %s GROUP BY %s ORDER BY cnt DESC LIMIT 20;",
                      columns[i, 1], table_name, columns[i, 1]);
  }

  #Third, execute all the column queries and dump the results to a file (table_name.txt)
  out_file <- paste(table_name, ".txt", sep="")
  write(sprintf("Output for %s table:\n", table_name), file = out_file, append = FALSE, sep = " ")
  for(i in 1:length(col_query)) {
    str <- sprintf("------------------------------------------------------------")
    print(str); write(str, file = out_file, append = TRUE, sep = " ")
    str <- sprintf("Query %d:%s", i, col_query[i])
    print(str); write(str, file = out_file, append = TRUE, sep = " ")
    col_result <- mysql_query(dbname, col_query[i])
    print(col_result)
    write.table(col_result, file = out_file, append = TRUE, row.names = FALSE, col.names = TRUE)
  }
}

#Analyze all the tables in a database
query_each_col_in_a_table(dbname, 'table1')
query_each_col_in_a_table(dbname, 'table2')
