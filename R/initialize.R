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
#' @importFrom DBI dbConnect dbDisconnect
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
  
  
  # users
  # User table for authentication
  # 
  # @param user (key) Character, username - used in: 
  #   `access`
  # @param password Character, password
  dbupdate(con, 'users', data$users, 'user')
  
  
  # access
  # User access table defining which experiments the user can access
  # 
  # @param user (key) Character, maps to `users$user`
  # @param expID (key) Character, maps to `expSummary$expID`
  dbupdate(con, 'access', data$access, c('user', 'expID'))

  
  # expSummary
  # Experiment summary table
  # 
  # @param expID (key) Character, experiment ID - used in: 
  #   `access`
  #   `expStats`
  #   `chanSummary`
  #   `chanRaw`
  #   `trackSummary`
  #   `trackRaw`
  # @param tracks_time Character, path to figure of tracks over time for each channel
  # @param tracks_v Character, path to figure of velocity over time for each grouping
  # @param angle_migration Character, path to figure of angle of migration viloin plots for each channel
  # @param ce Character, path to figure of chemotactic efficiency violin plots for each channel
  dbupdate(con, 'expSummary', data$expSummary, 'expID')

  
  # expStats
  # Table of summary statistics for each experiment
  # 
  # @param expID (key) Character, maps to `expSummary$expID`
  # @param within (key) Character, group for the comparison (i.e. within normals treated with fMLF8)
  # @param between (key) Character, contrast for the comparison (i.e. between channels 3 and 4)
  # @param test (key) Character, test used to compare the `between` groups
  # @param stat Double, test statistics comparing the `between` groups
  # @param p Double, p-value for `stat`
  dbupdate(con, 'expStats', data$expStats, c('expID', 'within', 'between', 'test'))

  
  # chanSummary
  # Channel summary table
  # 
  # @param expID (key) Character, maps to `expSummary$expID`
  # @param chanID (key) Integer, channel ID - used in:
  #   `chanRaw`
  #   `trackRaw`
  #   `trackSummary`
  # @param sID Character, sample ID - used in:
  # @param treatment Character, treatment applied to this channel
  # @param tot_finished Integer, Total number of cells that reached the bottom ledge
  # @param prop_finished Double, proportion of cells that reached the bottom ledge
  # @param ce_median Double, median chemotactic efficiency
  # @param ce_mean Double, mean chemotactic efficiency
  # @param ce_sd Double, standard deviation of chemotactic efficiency
  # @param angle_median Double, median angle of migration
  # @param angle_mean Double, mean angle of migration
  # @param angle_sd Double, standard deviation of angle of migration
  # @param max_v_median Double, median maximum velocity
  # @param max_v_mean Double, mean maximum velocity
  # @param max_v_sd Double, standard deviation of maximum velocity
  # @param dvud Double, dissimilarity score comparing directed and undirected trajectories
  # @param dvud_p Double, p-value for `dvud`
  dbupdate(con, 'chanSummary', data$chanSummary, c('expID', 'chanID'))
  
  
  # chanRaw
  # Table of smoothed trajectories over all tracks in a channel
  # 
  # @param expID (key) Character, maps to `expSummary$expID`
  # @param chanID (key) Integer, maps to `chanSummary$chanID`
  # @param x Double, smoothed x-position for the channel
  # @param y Double, smoothed y-position for the channel
  # @param frames (key) Integer, frame (sampled every 30 seconds)
  # @param v_x Double, velocity in the x direction (undirected)
  # @param v_y Double, velocity in the y direction (directed)
  # @param v Double, total velocity
  dbupdate(con, 'chanRaw', data$chanRaw, c('expID', 'chanID', 'frames'))

  
  # trackSummary
  # Track summary table
  # 
  # @param expID (key) Character, maps to `expSummary$expID`
  # @param chanID (key) Integer, maps to `chanSummary$chanID`
  # @param trackID (key) Integer, track ID - used in:
  #   `trackRaw`
  # @param ce Double, chemotactic efficiency
  # @param angle_migration Double, angle of migration
  # @param max_v Double, maximum velocity in μm per minute
  # @param av_velocity Double, mean velocity in μm per minute
  # @param finished  Logical, TRUE when the cell passed the bottom ledge
  dbupdate(con, 'trackSummary', data$trackSummary, c('expID', 'chanID', 'trackID'))

  
  # trackRaw
  # Raw track information
  # 
  # @param expID (key) Character, maps to `expSummary$expID`
  # @param chanID (key) Integer, maps to `chanSummary$chanID`
  # @param trackID (key) Integer, maps to `trackSummary$trackID`
  # @param x_px Double, x-position for the track in proportion of the channel width
  # @param y_px Double, y-position for the track in proportion of the channel width
  # @param x Double, x-position for the track in micrometers
  # @param y Double, y-position for the track in micrometers
  # @param frames (key) Integer, frame (sampled every 30 seconds)
  # @param time Double, time in minutes
  # @param v_x Double, velocity in the x direction (undirected) in micrometers per minute
  # @param v_y Double, velocity in the y direction (directed) in micrometers per minute
  # @param v Double, total velocity
  dbupdate(con, 'trackRaw', data$trackRaw, c('expID', 'chanID', 'trackID', 'frames'))

  
  # clean up
  dbDisconnect(con)
}


#' dbupdate
#' Update the database with new data. This will append new information or update non key fields.
#' 
#' @param con DBIConnection object
#' @param table Character value specifying the table to update.
#' @param dat A data.frame to update or add to the database.
#' @param key_fields Character vector specifying the key fields in the table.
#' 
#' @importFrom DBI dbAppendTable dbExecute dbGetQuery dbListTables dbWriteTable
#' @importFrom dplyr %>% ends_with right_join select
dbupdate <- function(con, table, dat, key_fields)
{
  # for all those pesky "no visible binding" notes
  if(FALSE)
    indb <- NULL
  
  # if dat is null, there is no data to deal with
  if(is.null(dat))
    return(invisible())
  
  # if the table doesn't exist, create it using dat and return
  if(!table %in% dbListTables(con))
    dbWriteTable(con, table, dat) %>%
      invisible()


  # get non-key fields
  non_key_fields <- names(dat)[!names(dat) %in% key_fields]
  

  # if there are no non-key fields, we need to do things differently
  if(length(non_key_fields) == 0)
  {
    # get the key fields from the database
    tmp <- dbGetQuery(con, paste("SELECT * FROM", table)) %>%
      mutate(indb = TRUE) %>%                               # flag these as already in the database
      
      right_join(dat, by = key_fields) %>%                  # join the new data to the database
      
      mutate(indb = ifelse(is.na(indb), FALSE, indb)) %>%   # flag these as not in the database
      filter(!indb) %>%                                     # only keep the rows that are not in the database
      select(-indb)
    
    # if there are new rows, append to the database
    if(nrow(tmp) > 0)
    {
      # append the new rows to the database
      dbAppendTable(con, table, tmp)
    }
    
    # no more work to be done, return
    invisible()
    
  # if we do have non-key fields, look to see what can be updated vs what needs to be appended
  }else{
    # if the table exists, check for new data and rows that need updating
    tmp <- dbGetQuery(con, paste("SELECT * FROM", table)) %>%
      right_join(dat, by = key_fields)
    
    
    
    # check if we want to append this row (if non-key fields are NA - assume no missing data in database)
    apnd <- is.na(tmp[[paste0(non_key_fields[1], '.x')]])
    
    
    # check for rows that need updating
    updt <- rep(FALSE, nrow(tmp)) # this will be determined below
    
    for(i in non_key_fields)
    {
      # check if we want to update this row (if non-key fields are not equal)
      comparison <- tmp[[paste0(i, '.x')]] != tmp[[paste0(i, '.y')]]
      updt <- updt | ifelse(is.na(comparison), FALSE, comparison)
      
      # we'll pretty much always want the .y variable (new data) if it exists
      tmp[[i]] <- tmp[[paste0(i, '.y')]]
    }  
    
    
    # append new rows
    if(any(apnd))
    {
      tmp[apnd,] %>%
        select(-ends_with('.x'),
               -ends_with('.y')) %>%
        dbAppendTable(conn = con, name = table)
    }
    
    
    # update existing rows
    if(any(updt))
    {
      # add quotes around character values
      for(i in names(tmp))
      {
        if(is.character(tmp[[i]]))
          tmp[[i]] <- paste0("'", tmp[[i]], "'")
      }
      
      # update rows
      for(i in (1:nrow(tmp))[updt])
      {
        dbExecute(con, paste("UPDATE", table,
                             "SET", paste(paste0(non_key_fields, '=', tmp[i, non_key_fields]), collapse = ', '),
                             "WHERE", paste(paste0(key_fields, '=', tmp[i, key_fields]))))
      }
    }
    
    invisible()
  }
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
