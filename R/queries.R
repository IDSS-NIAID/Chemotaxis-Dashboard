# queries.R
# SQL queries to fetch data from the database

#' get_dat
#' Get data from the database
#' 
#' @param con DBI connection specifying the database to pull from
#' @param select Character value specifying the columns to pull
#' @param from Character value defining which table to pull from
#' @param where Character value that will get put into the WHERE part of the SQL statement
#' 
#' @return A data.frame with the requested data
#' @export
#' @importFrom DBI dbGetQuery
get_dat <- function(con, select = '*', from, where = NULL)
{
  # build query
  query <- paste("SELECT", select, "FROM", from)
  
  # add where clause if it exists
  if(!is.null(where))
    query <- paste(query, 'WHERE', where)
  
  dbGetQuery(con, query)
}


#' remove_dups
#' Remove duplicate rows from the database
#' 
#' @param con A DBI database connection object
#' @param tabs A character vector of table names to remove duplicates from. If NULL, all tables will be checked.
#' 
#' @export
#' @importFrom DBI dbGetQuery
#' @importFrom DBI dbListTables
#' @importFrom DBI dbWriteTable
#' @importFrom dplyr %>%
remove_dups <- function(con, tabs = NULL)
{
  if(is.null(tabs))
    tabs <- dbListTables(con)
  
  for(t in tabs)
  {
    dbGetQuery(con, paste("SELECT * FROM", t)) |>
      unique() |>
      dbWriteTable(conn = con, name = t, overwrite = TRUE)
  }
}