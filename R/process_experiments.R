# process_experiments.R
# Functions included: 
#  preprocess_experiments()
#  one_experiment()
#  compare_two_functions()


#' preprocess_experiments 
#' This will read in results from the convolutional model and preprocess for the dashboard
#' 
#' @param experiment Date of the experiment to run. Argument should be of the form: `experiment="\%Y\%m\%d"`.
#' @param source_dir Path to directory containing raw csv tracks files
#' @param results_dir Path to processed data directory
#' @param ledge_dist Numeric, distance between top and bottom ledges of the microscope image in micrometers (default is 260)
#' @param ledge_upper Numeric, location of the upper ledge in the raw Y coordinate system (default is 100)
#' @param ledge_lower Numeric, location of the lower ledge in the raw Y coordinate system (default is 500)
#' 
#' @details Work performed in `compare_two_functions`, called by this function, uses the `doParallel` package.
#' Using multiple cores will speed this step up significantly. To do so, you will need to make a cluster and 
#' register it with `doParallel` as follows: 
#' 
#' `my.cluster <- parallel::makeCluster(n.cores, type = "PSOCK")`
#' `doParallel::registerDoParallel(cl = my.cluster)`
#' 
#' where `n.cores` is the number of cores you with to allocate to the task.
#' A good starting point is `parallel::detectCores() - 1`.
#' 
#' If this is not done, the code will execute serially.
#' 
#' @return A list of data.frames...
#' 
#' @examples 
#' # Here is an example line for the swarm file:
#' # `module load R; R -e \
#' #   "ChemotaxisDashboard::process_experiments('20070308', \
#' #                                             '~/chemodash_raw', \
#' #                                             '~/chemodash_out')"`
#' @export
#' 
#' @importFrom dplyr bind_rows left_join mutate select tibble
#' @importFrom purrr map map_chr map_int map_df map2_chr map2_df
#' @importFrom readr read_csv
#' @importFrom stringr str_replace
#' @importFrom tidyr unnest
process_experiments <- function(experiment, source_dir, results_dir,
                                ledge_dist = 260, ledge_upper = 100, ledge_lower = 500)
{
  # for all those pesky "no visible binding" notes
  if(FALSE)
    channel <- dup <- key <- X <- Y <- NULL
  
  options(dplyr.summarise.inform = FALSE)

  # source files for processing
  f <- list.files(source_dir)
  f <- map(experiment, ~ grep(.x, f, value = TRUE)) |>
    unlist() |>
    unique()
  
  if(length(f) == 0)
  {
    warning('Skipping ', experiment, ' - no results found.')
    return(NULL)
  }

  ##### experiment metadata #####
  results_meta <- tibble(
    f = f,

    dat = strsplit(f, '_', fixed = TRUE),

    date = {map_chr(dat, `[`, 1) |>
        substr(1, 8) |>
        as.Date(format = '%Y%m%d')},
    
    experiment = map_chr(dat, `[`, 1) |>
      str_replace('-$', ''),
    
    channel = map_int(dat, ~
                        {grep(.x, pattern = 'CH[1-6]', value = TRUE) |>
                            substr(3, 3) |>
                            as.integer()}),

    sample = map_chr(dat, ~
                       {.x[.x != ''][-1] |>
                           gsub(pattern = '.csv', replacement = '', fixed = TRUE) |>
                           grep(pattern = '^[0-9]', invert = TRUE, value = TRUE) |> # drop any single digit numbers
                           grep(pattern = 'CH[1-6]', invert = TRUE, value = TRUE) |> # drop channel
                           grep(pattern = 'fMLF|Basal|Buffer|C5a|SDF|IL.|LTB4', ignore.case = TRUE, invert = TRUE, value = TRUE)}[1]), # drop attractant
                           
    treatment = map_chr(dat, ~
                          {.x |>
                              gsub(pattern = '.csv', replacement = '', fixed = TRUE) |>
                              grep(pattern = 'fMLF|Basal|Buffer|C5a|SDF|IL.|LTB4',
                                   ignore.case = TRUE, value = TRUE)}[1])) |>

    mutate(sample = tolower(sample),   # inconsistent capitalization
           key = paste(experiment, channel),
           dup = duplicated(key),
           experiment = paste0(experiment, ifelse(dup, 'a',''))) |>

    dplyr::select(-dat, -key)
  
  # note any experiments that were run on the same day
  if(any(results_meta$dup))
  {
    experiment <- c(experiment, unique(results_meta$experiment[results_meta$dup]))
  }
  
  results_meta <- dplyr::select(results_meta, -dup)


  ##### read in raw data #####
  dat <- map_df(f, ~ read_csv(file.path(source_dir, .x), col_types = 'dddd') |> mutate(f = .x)) |>

    left_join(results_meta, by = 'f') |>

    filter(!is.na(X) & !is.na(Y))

  
  ##### pre-process data for these experiments #####
  retval <- unique(dat$experiment) |>
    map(~ one_experiment(dat_sub = filter(dat, experiment == .x),
                         experiment = .x,
                         results_dir = results_dir,
                         ledge_dist = ledge_dist,
                         ledge_upper = ledge_upper,
                         ledge_lower = ledge_lower))

  list(chanSummary  = map_df(retval, ~ .x$chanSummary),
       trackRaw     = map_df(retval, ~ .x$trackRaw))
}


#' Analysis of data for each experiment
#' 
#' @param dat_sub data frame (tibble) containing the subset of data for a single experiment
#' @param experiment Date of the experiment to run. Argument should be of the form: `experiment="\%Y\%m\%d"`.
#' @param results_dir path to directory for saving data
#' @param ledge_dist Numeric, distance between top and bottom ledge of the microscope image in micrometers (default is 260)
#' @param ledge_upper Numeric, location of the upper ledge in the raw Y coordinate system (default is 100). This should be the first ledge crossed.
#' @param ledge_lower Numeric, location of the lower ledge in the raw Y coordinate system (default is 500). This should be the second ledge crossed.
#' 
#' @return A list of data.frames containing summaries and raw data from the data in dat_sub.
#' @export
#' @import rlang
#' 
#' @importFrom dplyr arrange case_when distinct filter group_by left_join mutate n_distinct reframe rename select starts_with summarize ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map map_dbl map2 map2_dbl map2_lgl pmap
#' @importFrom stats lm median quantile sd smooth.spline splinefun na.omit
#' @importFrom utils combn
one_experiment <- function(dat_sub, experiment, results_dir, ledge_dist = 260, ledge_upper = 100, ledge_lower = 500)
{
  # for all those pesky "no visible binding" notes
  if(FALSE)
  {
    angle_mean <- angle_median <- angle_migration <- angle_sd <- av_theta <- ce <- ce_mean <- ce_median <- ce_sd <- NULL
    channel <- channel_a <- cross_at <- d <- d1 <- d2 <- delta_x <- delta_y <- directed_v_undirected <- distance_traveled <- NULL
    dvud <- dvud_p <- experiment <- finish_at <- finished <- Frame <- frames <- grp <- key_violation <- l <- max_y <- NULL
    max_v <- max_v_mean <- max_v_median <- max_v_sd <- minutes <- nchannels <- nsamps <- ntrts <- prop_finished <- sd_theta <- NULL
    theta <- theta_median <- theta_mean <- theta_sd <- tot_finished <- Track <- treatment <- v <- v_x <- v_y <- NULL
    X <- x <- Y <- y <- y_max <- y_min <- NULL
    Xo <- f <- non_mover <- pre_start <- post_end <- pass_filters <- raw_distance <- all_pre_start <- NULL
    observe_finish <- observe_start <- tab <- n_cells <- no_start <- n_finished <- NULL
  }
  
  # make sure we aren't getting more than 6 channels (indicates we have a day with multiple experiments that we missed)
  if(n_distinct(dat_sub$f) > 6)
    paste('More than 6 channels detected. This is likely due to multiple experiments being run on the same day. Please check the data and try again.',
          paste(unique(dat_sub$f), collapse = ', ')) |>
    stop()
  
  
  ##################################
  # Prep dat_sub for summarization #
  ##################################
  
  dat_sub <- dat_sub |>
    
    unique() |> # remove duplicates - don't want them to be flagged as a key_violation below
    
    # should already be sorted, but just to be sure...
    arrange(experiment, channel, Track, Frame) |>
    
    # look for cells that are in two places at once (this compares row i with row i+1)
    mutate(key_violation = c(Frame[-n()] == Frame[-1], FALSE) &
             c(Track[-n()] == Track[-1], FALSE)) |>
  
    filter(!key_violation &                                      # matches the row after this
           !c(FALSE, key_violation[-length(key_violation)])) |> # matches the row previous to this
  
    group_by(channel, Track) |>
    mutate(
      # this is the frame where the cell first crosses the upper ledge
      cross_at = case_when(    Y[1] >= ledge_upper  ~ Frame[1],
                           
                           # if it never crosses the top ledge, use the last frame as the starting point
                           all(Y    <  ledge_upper) ~ Frame[n()], 
                           
                           # if it crosses the top ledge, find the first frame where it crosses
                           TRUE           ~ suppressWarnings(min(Frame[c(FALSE, Y[-length(Y)] <  ledge_upper &
                                                                                Y[-1]         >= ledge_upper)], na.rm = TRUE))),
    
      # this is the frame where the cell first crosses the bottom ledge
      finish_at = case_when(# if it never crosses the bottom ledge, use the last frame as the ending point
                            all(Y < ledge_lower) ~ Frame[length(Y)],
                            
                            # if it isn't tracked until after it crosses the lower ledge, use the first frame as the ending point
                            all(Y > ledge_lower) | Y[1] > ledge_lower ~ Frame[1],
                            
                            # if it crosses the bottom ledge, find the first frame below the threshold
                            TRUE           ~ suppressWarnings(min(Frame[Y >= ledge_lower]))),
      
      # track cells that we observe crossing the top/bottom ledges
      #   (or that started across the ledge and are reasonably close to the top ledge in the first image)
      observe_start = any(Y >= ledge_upper) & (any(Y < ledge_upper) | any(cross_at == 1 & Y < 0.05)),
      observe_finish = any(Y >= ledge_lower) & any(Y < ledge_lower),
      
      # calculate starting X position for each track
      Xo = X[Frame == cross_at]
    ) |>
    ungroup() |>
    
    mutate(
      # translate X st each cell starts at (0,~0) when first crossing top ledge
      x = X - Xo,

      # convert to micrometers
      x =  x                * ledge_dist / (ledge_lower - ledge_upper),
      y = (Y - ledge_upper) * ledge_dist / (ledge_lower - ledge_upper), # center at upper ledge before converting to micrometers
      
      y_min = min(y),
      y_max = max(y),
      
      # remove '.csv' from file names
      f = gsub('.csv', '', f, fixed = TRUE),

      # filter out tracks and frames that we don't want to include in the analysis
      non_mover  = y_max - y_min < 1,                                                     # drop any "cells" that don't move at all (+/- a few pixels / at least one micrometer)
      pre_start  = Frame < cross_at | y < 0,                                              # frames prior to crossing the upper ledge
      no_start   = y_max < 0,                                                             # as well as any tracks that never cross the upper ledge
      post_end   = Frame > finish_at | y > ledge_dist,                                    # frames after crossing the lower ledge
      drop       = ifelse(is.na(treatment), FALSE, treatment == 'fMLF (did not work)') |  # drop this one that didn't work
                   non_mover | pre_start | no_start | post_end) |>
    
    # calculate additional summary statistics only with frames that pass the filters above
    group_by(channel, Track) |>
    mutate(pass_filters = sum(!drop), # count number of frames we have data for each track (and pass the filtering requirements above)
           
           # raw distance
           raw_distance = c(0, diff(x[!drop])^2 + diff(y[!drop])^2) |> # add the 0 to avoid empty vectors
             sqrt() |>
             sum(),
           
           ##### calculate angle of migration at each time point #####
           # d1 is the angle from the previous frame to the current frame
           d1 = c(NA, atan2(y[-1] - y[-n()], x[-1] - x[-n()])),
             
           # d2 is the angle from the current frame to the next frame
           d2 = c(d1[-1], NA),
           
           # theta is the average of d1 and d2
           theta = map2_dbl(d1, d2, ~ mean(c(.x, .y), na.rm = TRUE)) / pi * 180) |>
    dplyr::select(-d1, -d2) |>
    ungroup()

  ### count dropped records ###
  
  # tracks that didn't move
  non_movers <- dat_sub |>
    dplyr::select(channel, Track, non_mover) |>
    dplyr::distinct() |>
    group_by(channel) |>
    dplyr::summarize(non_movers = sum(non_mover)) |>
    ungroup()
  
  # tracks that never started
  dns <- dat_sub |>
    dplyr::select(channel, Track, pre_start) |>
    dplyr::distinct() |>
    group_by(channel, Track) |>
    mutate(all_pre_start = all(pre_start)) |>
    ungroup() |>
    group_by(channel) |>
    dplyr::summarize(dns = sum(all_pre_start)) |>
    ungroup()
  
  # frames before the start
  pre_start_frames <- group_by(dat_sub, channel, Track) |>
    summarize(pre_start_frames = sum(pre_start)) |>
    ungroup() |>
    dplyr::filter(pre_start_frames > 0)
  
  # frames after the end
  post_end_frames <- group_by(dat_sub, channel, Track) |>
    summarize(post_end_frames = sum(post_end)) |>
    ungroup() |>
    dplyr::filter(post_end_frames > 0)
  
  ### drop records ###
  dat_sub <- dat_sub |>
    dplyr::filter(!drop) |>
    dplyr::select(-key_violation, -drop, -non_mover, -pre_start, -no_start, -post_end, -raw_distance)


  ############
  # Velocity #
  ############

  track_smooth <- dat_sub |>
    group_by(experiment, channel, Track, sample, treatment) |>
    summarize(
      # calculate smooth functions of x, y, and theta over time
      x     = map2(list(Frame), list(x), ~
                     {
                       if(length(.x) < 4)
                        return(splinefun(.x, .y))
                       
                       tmp <- smooth.spline(.x, .y)
                       splinefun(tmp$x, tmp$y)
                     }),
      y     = map2(list(Frame), list(y), ~
                     {
                       if(length(.x) < 4)
                        return(splinefun(.x, .y))
                       
                       tmp <- smooth.spline(.x, .y)
                       splinefun(tmp$x, tmp$y)
                     }),
      theta = map2(list(Frame), list(theta), ~
                     {
                       if(length(.x) < 4)
                        return(splinefun(.x, .y))
                       
                       tmp <- smooth.spline(.x, .y)
                       splinefun(tmp$x, tmp$y)
                     }),
      Frame = map(list(Frame), ~ unique(.x))) |>
    ungroup() |>
    mutate(
      # calculate velocity over time (multiply by 2 to calculate velocity in micrometers  per minute - frames are every 30 seconds)
      v_x = map2(x, Frame, function(x, f) x(f, deriv = 1) * 2),
      v_y = map2(y, Frame, function(y, f) y(f, deriv = 1) * 2),
      v = map2(v_x, v_y, ~ sqrt(.x^2 + .y^2) * sign(.y)), # going down = positive velocity, going up = negative velocity
      
      # convert functions of x, y, and theta back to values (smoothed)
      x     = map2(    x, Frame, function(    x, f)     x(f)),
      y     = map2(    y, Frame, function(    y, f)     y(f)),
      theta = map2(theta, Frame, function(theta, f) theta(f))) |>
    
    unnest(c(x, y, theta, Frame, v_x, v_y, v))

  
  ##########################
  # Channel-level metadata #
  ##########################
  
  channel_summ <- select(dat_sub, date, experiment, channel, sample, treatment,
                         cross_at, finish_at, observe_start, observe_finish) |>
    unique()
  

  ##### Return Results #####
  
  # compare this output with the comments in `initialize.R`
  list(chanSummary = channel_summ |>
         rename(expID  = experiment, 
                chanID = channel,
                sID    = sample),
       
       trackRaw = dat_sub |>
         select(experiment, channel, Track, Frame, X, Y) |>
         rename(x_px = X, y_px = Y) |>
         left_join(track_smooth, by = join_by(experiment, channel, Track, Frame)) |>
         rename(expID = experiment,
                chanID = channel,
                trackID = Track,
                frames = Frame) |>
         mutate(time = frames / 2) |>
         select(expID, 
                chanID,
                trackID,
                x_px,
                y_px,
                x,
                y,
                frames,
                time,
                v_x,
                v_y,
                v,
                theta)
       )
}
