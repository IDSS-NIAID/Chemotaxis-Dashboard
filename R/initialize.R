# initialize.R

#' dbinit
#' Set up database and infrastructure needed to run the Chemotaxis-Dashboard. If the database already exists and data is not empty, this will append the information in data to the database. 
#' 
#' @param db_path Character value specifying the path to the file where the database should be initialized.
#' @param data List of data.frames to initialize or add to the database. Internal test data will be used if `data` is NULL.
#' @param con A database connection (optional) if one is already open
#'
#' @details The data.frames expected in data are `expStats`, `chanSummary`, `chanRaw`,
#' `trackSummary`, and `trackRaw` (as returned by `process_experiments()`).
#' 
#' @return Something - use this to sniff for existing DB information?
#' @export
#' 
#' @importFrom DBI dbConnect dbDisconnect
#' @importFrom RSQLite SQLite
dbinit <- function(db_path = NULL, data = NULL, con = NULL)
{
  # for testing:
  # db_path <- file.path(system('git rev-parse --show-toplevel', intern = TRUE), '.data', 'chemo-dash.sqlite')
  
  # fill with test data if none is provided
  if(is.null(data))
    data <- get_test_data()

  # connect
  if(is.null(con))
  {
    con <- dbConnect(SQLite(), db_path)
    disconnect <- TRUE
  }else{
    disconnect <- FALSE
  }
  

  # chanSummary
  # Channel summary table
  # 
  # @param expID      (key) Character, maps to `expSummary$expID`
  # @param chanID     (key) Integer, channel ID - used in:
  #                           `chanRaw`
  #                           `trackRaw`
  #                           `trackSummary`
  # @param sID              Character, sample ID
  # @param treatment        Character, treatment applied to this channel

  ### these will be calculated on the fly ###
  # @param tot_finished     Integer, Total number of cells that reached the bottom ledge
  # @param prop_finished    Double, proportion of cells that reached the bottom ledge
  # @param n_cells          Integer, estimated number of cells in the channel
  # @param ce_median        Double, median chemotactic efficiency
  # @param ce_mean          Double, mean chemotactic efficiency
  # @param ce_sd            Double, standard deviation of chemotactic efficiency
  # @param angle_median     Double, median angle of migration (net)
  # @param angle_mean       Double, mean angle of migration (net)
  # @param angle_sd         Double, standard deviation of angle of migration (net)
  # @param theta_median     Double, median instantaneous angle of migration
  # @param theta_mean       Double, mean instantaneous angle of migration
  # @param theta_sd         Double, standard deviation of instantaneous angle of migration
  # @param max_v_median     Double, median maximum velocity
  # @param max_v_mean       Double, mean maximum velocity
  # @param max_v_sd         Double, standard deviation of maximum velocity
  # @param non_movers       Integer, number of tracks removed because they do not move
  # @param little_movement  Integer, number of tracks removed because they move very little
  # @param dns              Integer, number of tracks removed bacause they do not cross the upper threshold
  # @param few_frames       Integer, number of tracks removed because they have 3 or fewer frames
  # @param pre_start_frames Integer, number of frames removed prior to a track crossing the upper threshold
  # @param post_end_frames  Integer, number of frames removed after a track crosses the bottom threshold

  dbupdate(con,
           table = 'chanSummary',
           dat = data$chanSummary,
           key_fields = c('expID', 'chanID'))
  
  
  # chanRaw
  # Table of smoothed trajectories over all tracks in a channel
  # Deprecated: these will actually be calculated on the fly
  # 
  # @param expID  (key) Character, maps to `expSummary$expID`
  # @param chanID (key) Integer, maps to `chanSummary$chanID`
  # @param x            Double, smoothed x-position for the channel
  # @param y            Double, smoothed y-position for the channel
  # @param frames (key) Integer, frame (sampled every 30 seconds)
  # @param v_x          Double, velocity in the x direction (undirected)
  # @param v_y          Double, velocity in the y direction (directed)
  # @param v            Double, total velocity
  # @param theta        Double, smoothed angle of migration at each frame

  
  # trackRaw
  # Raw track information
  # 
  # @param expID   (key) Character, maps to `expSummary$expID`
  # @param chanID  (key) Integer, maps to `chanSummary$chanID`
  # @param trackID (key) Integer, maps to `trackSummary$trackID`
  # @param x_px          Double, x-position for the track in proportion of the channel width
  # @param y_px          Double, y-position for the track in proportion of the channel width
  # @param x             Double, x-position for the track in micrometers
  # @param y             Double, y-position for the track in micrometers
  # @param frames  (key) Integer, frame (sampled every 30 seconds)
  # @param time          Double, time in minutes
  # @param v_x           Double, velocity in the x direction (undirected) in micrometers per minute
  # @param v_y           Double, velocity in the y direction (directed) in micrometers per minute
  # @param v             Double, total velocity
  # @param theta         Double, instantaneous angle of migration
  dbupdate(con,
           table = 'trackRaw',
           dat = data$trackRaw,
           key_fields = c('expID', 'chanID', 'trackID', 'frames'))

  
  # clean up
  if(disconnect)
    dbDisconnect(con)
  
  invisible()
}


#' dbupdate
#' Update the database with new data. This will append new information or update non key fields.
#' 
#' @param con DBIConnection object
#' @param table Character value specifying the table to update.
#' @param dat A data.frame to update or add to the database.
#' @param key_fields Character vector specifying the key fields in the table.
#' 
#' @export
#' @importFrom DBI dbAppendTable dbExecute dbGetQuery dbListTables dbWriteTable
#' @importFrom dplyr ends_with right_join select
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
  {
    dbWriteTable(con, table, dat)
    return(invisible())
  }

  # get non-key fields
  non_key_fields <- names(dat)[!names(dat) %in% key_fields]
  

  # if there are no non-key fields, we need to do things differently
  if(length(non_key_fields) == 0)
  {
    # get the key fields from the database
    tmp <- dbGetQuery(con, paste("SELECT * FROM", table)) |>
      mutate(indb = TRUE) |>                               # flag these as already in the database
      
      right_join(dat, by = key_fields) |>                  # join the new data to the database
      
      mutate(indb = ifelse(is.na(indb), FALSE, indb)) |>   # flag these as not in the database
      filter(!indb) |>                                     # only keep the rows that are not in the database
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
    tmp <- dbGetQuery(con, paste0("SELECT * FROM ", table, 
                                  " WHERE expID IN ('", paste(unique(dat$expID), collapse = "', '"), "')")) |>
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
      tmp[apnd,] |>
        select(-ends_with('.x'),
               -ends_with('.y')) |>
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
                             "WHERE", paste(paste0(key_fields, '=', tmp[i, key_fields]), collapse = ' AND ')))
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
  list(chanSummary  = chanSummary,
       trackRaw     = trackRaw)
}
