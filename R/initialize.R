# initialize.R

#' dbinit
#' Set up database and infrastructure needed to run the Chemotaxis-Dashboard. If the database already exists and data is not empty, this will append the information in data to the database. 
#' 
#' @param db_path Character value specifying the path to the file where the database should be initialized.
#' @param data List of data.frames to initialize or add to the database. Internal test data will be used if `data` is NULL.
#'
#' @details The data.frames expected in data are `users`, `access`, `expSummary`, `expStats`, `chanSummary`, `chanRaw`,
#' `trackSummary`, and `trackRaw` (as returned by `process_experiments()`).
#' 
#' @return Something - use this to sniff for existing DB information?
#' @export
#' 
#' @importFrom DBI dbAppendTable
#' @importFrom DBI dbConnect
#' @importFrom DBI dbDisconnect
#' @importFrom DBI dbListTables
#' @importFrom DBI dbWriteTable
#'
#' @importFrom RSQLite SQLite
dbinit <- function(db_path, data = NULL)
{
  # for testing:
  # db_path <- file.path(system('git rev-parse --show-toplevel', intern = TRUE), '.data', 'chemo-dash.sqlite')
  
  # fill with test data if none is provided
  if(is.null(data))
    data <- get_test_data()

  # connect
  con <- dbConnect(SQLite(), db_path)
  
  # get list of existing tables
  tabs <- dbListTables(con)
  
  # add data.frames
  if(!'users' %in% tabs & 
     !is.null(data$users))
  {
    dbWriteTable(con, "users", data$users)
  }else if(!is.null(data$users)){
    dbAppendTable(con, "users", data$users)
  }
  
  if(!'access' %in% tabs & 
     !is.null(data$access))
  {
    dbWriteTable(con, "access", data$access)
  }else if(!is.null(data$access)){
    dbAppendTable(con, "access", data$access)
  }
  
  if(!'expSummary' %in% tabs & 
     !is.null(data$expSummary))
  {
    dbWriteTable(con, "expSummary", data$expSummary)
  }else if(!is.null(data$expSummary)){
    dbAppendTable(con, "expSummary", data$expSummary)
  }
  
  if(!'expStats' %in% tabs & 
     !is.null(data$expStats))
  {
    dbWriteTable(con, "expStats", data$expStats)
  }else if(!is.null(data$expStats)){
    dbAppendTable(con, "expStats", data$expStats)
  }
  
  if(!'chanSummary' %in% tabs & 
     !is.null(data$chanSummary))
  {
    dbWriteTable(con, "chanSummary", data$chanSummary)
  }else if(!is.null(data$chanSummary)){
    dbAppendTable(con, "chanSummary", data$chanSummary)
  }
  
  if(!'chanRaw' %in% tabs & 
     !is.null(data$chanRaw))
  {
    dbWriteTable(con, "chanRaw", data$chanRaw)
  }else if(!is.null(data$chanRaw)){
    dbAppendTable(con, "chanRaw", data$chanRaw)
  }

  if(!'trackSummary' %in% tabs & 
     !is.null(data$trackSummary))
  {
    dbWriteTable(con, "trackSummary", data$trackSummary)
  }else if(!is.null(data$trackSummary)){
    dbAppendTable(con, "trackSummary", data$trackSummary)
  }
  
  if(!'trackRaw' %in% tabs & 
     !is.null(data$trackRaw))
  {
    dbWriteTable(con, "trackRaw", data$trackRaw)
  }else if(!is.null(data$trackRaw)){
    dbAppendTable(con, "trackRaw", data$trackRaw)
  }
  
  dbDisconnect(con)
}


#' get_test_data
#' Get a list of built-in test data from the package
#' 
#' @return A list of test data as defined in `data-raw/process_raw_data.R`
#' @export
get_test_data <- function()
{
  list(users        = users,
       access       = access,
       expSummary   = expSummary,
       expStats     = expStats,
       chanSummary  = chanSummary,
       chanRaw      = chanRaw,
       trackSummary = trackSummary,
       trackRaw     = trackRaw)
}