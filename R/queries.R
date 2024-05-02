# queries.R
# SQL queries to fetch data from the database

#' get_dat
#' Get data from the database, recpecting permissions
#' 
#' @param con DBI connection specifying the database to pull from
#' @param user Character value specifying the username
#' @param select Character value specifying the columns to pull
#' @param from Character value defining which table to pull from
#' @param where Character value that will get put into the WHERE part of the SQL statement
#' 
#' @return A data.frame with the requested data
#' @export
#' @importFrom DBI dbGetQuery
#' 
#' @importFrom dplyr %>%
#' @importFrom dplyr join_by
#' @importFrom dplyr inner_join
get_dat <- function(con, user, select = '*', from, where = NULL)
{
  # pesky binding warning
  if(FALSE)
    expID <- NULL
  
  query <- paste("SELECT", select, "FROM", from)
  
  if(!is.null(where))
    query <- paste(query, 'WHERE', where)
  
  dbGetQuery(con, paste0("SELECT expID FROM access WHERE user='", user, "'")) %>%
    inner_join(dbGetQuery(con, query), by = join_by(expID))
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
remove_dups <- function(con, tabs = NULL)
{
  if(is.null(tabs))
    tabs <- dbListTables(con)
  
  for(t in tabs)
  {
    dbGetQuery(con, paste("SELECT * FROM", t)) %>% 
      unique() %>% 
      dbWriteTable(conn = con, name = t, overwrite = TRUE)
  }
}