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
#' @param seed Random seed to use
#' @param sig.figs Number of significant figures to pass to one_experiment`
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
#' @importFrom dplyr %>% bind_rows left_join mutate select tibble
#' @importFrom purrr map map_chr map_int map_df map2_chr map2_df
#' @importFrom readr read_csv
#' @importFrom stringr str_replace
process_experiments <- function(experiment, source_dir, results_dir, seed = NULL, sig.figs = 4,
                                ledge_dist = 260, ledge_upper = 100, ledge_lower = 500)
{
  # for all those pesky "no visible binding" notes
  if(FALSE)
    channel <- dup <- key <- X <- Y <- NULL
  
  options(dplyr.summarise.inform = FALSE)

  # source files for processing
  f <- list.files(source_dir)
  f <- map(experiment, ~ grep(.x, f, value = TRUE)) %>%
    unlist() |>
    unique()
  

  ##### experiment metadata #####
  results_meta <- tibble(
    f = f,

    dat = strsplit(f, '_', fixed = TRUE),

    date = {map_chr(dat, `[`, 1) %>%
        substr(1, 8) %>%
        as.Date(format = '%Y%m%d')},
    
    experiment = map_chr(dat, `[`, 1) |>
      str_replace('-$', ''),
    
    channel = map_int(dat, ~
                        {grep(.x, pattern = 'CH[1-6]', value = TRUE) %>%
                            substr(3, 3) %>%
                            as.integer()}),

    sample = map_chr(dat, ~
                       {.x[.x != ''][-1] %>%
                           gsub(pattern = '.csv', replacement = '', fixed = TRUE) %>%
                           grep(pattern = '^[0-9]$', invert = TRUE, value = TRUE) %>% # drop any single digit numbers
                           grep(pattern = 'CH[1-6]', invert = TRUE, value = TRUE) %>% # drop channel
                           grep(pattern = 'fMLF|Basal|Buffer|C5a|SDF|IL.|LTB4', ignore.case = TRUE, invert = TRUE, value = TRUE)}[1]), # drop attractant
                           
    treatment = map_chr(dat, ~
                          {.x %>%
                              gsub(pattern = '.csv', replacement = '', fixed = TRUE) %>%
                              grep(.x, pattern = 'fMLF|Basal|Buffer|C5a|SDF|IL.|LTB4',
                                   ignore.case = TRUE, value = TRUE)}[1])) %>%

    mutate(sample = tolower(sample),   # inconsistent capitalization
           key = paste(experiment, channel),
           dup = duplicated(key),
           experiment = paste0(experiment, ifelse(dup, 'a',''))) %>%

    dplyr::select(-dat, -key)
  
  # note any experiments that were run on the same day
  if(any(results_meta$dup))
  {
    experiment <- c(experiment, unique(results_meta$experiment[results_meta$dup]))
  }
  
  results_meta <- dplyr::select(results_meta, -dup)


  ##### read in raw data #####
  dat <- map2_df(source_dir, f, ~
                   {
                     paste(.x, .y, sep = '/') %>%
                       read_csv(col_types = 'dddd') %>%
                       mutate(f = .y)
                   }) %>%

    bind_rows() %>%

    left_join(results_meta, by = 'f') %>%

    filter(!is.na(X) & !is.na(Y))

  
  ##### pre-process data for these experiments #####
  retval <- unique(dat$experiment) %>%
    map(~ one_experiment(dat_sub = filter(dat, experiment == .x),
                         experiment = .x,
                         results_dir = results_dir, 
                         seed = seed,
                         sig.figs = sig.figs,
                         ledge_dist = ledge_dist,
                         ledge_upper = ledge_upper,
                         ledge_lower = ledge_lower))

  list(expSummary   = map_df(retval, ~ .x$expSummary),
       expStats     = map_df(retval, ~ .x$expStats),
       chanSummary  = map_df(retval, ~ .x$chanSummary),
       chanRaw      = map_df(retval, ~ .x$chanRaw),
       trackSummary = map_df(retval, ~ .x$trackSummary),
       trackRaw     = map_df(retval, ~ .x$trackRaw))
}


#' Analysis of data for each experiment
#' 
#' @param dat_sub data frame (tibble) containing the subset of data for a single experiment
#' @param experiment Date of the experiment to run. Argument should be of the form: `experiment="\%Y\%m\%d"`.
#' @param results_dir path to directory for saving data
#' @param seed seed for random number generation
#' @param sig.figs maximum significant digits for p-values obtained by permutation testing
#' @param ledge_dist Numeric, distance between top and bottom ledge of the microscope image in micrometers (default is 260)
#' @param ledge_upper Numeric, location of the upper ledge in the raw Y coordinate system (default is 100). This should be the first ledge crossed.
#' @param ledge_lower Numeric, location of the lower ledge in the raw Y coordinate system (default is 500). This should be the second ledge crossed.
#' 
#' @return A list of data.frames containing summaries and raw data from the data in dat_sub.
#' @export
#' @import rlang
#' @importFrom magrittr %>%
#' 
#' @importFrom dplyr arrange case_when distinct filter group_by left_join mutate n_distinct reframe rename select starts_with summarize ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map map_dbl map2 map2_dbl map2_lgl pmap
#' @importFrom stats lm median quantile sd smooth.spline splinefun
#' @importFrom splines bs
#' @importFrom utils combn
#' @importFrom ggplot2 aes ggplot element_blank facet_wrap geom_boxplot geom_jitter geom_hline geom_path geom_violin ggsave scale_y_reverse scale_color_gradient2 scale_color_manual stat_smooth theme theme_set xlab ylab
#' @importFrom cowplot theme_cowplot
#' @importFrom grDevices rgb
one_experiment <- function(dat_sub, experiment, results_dir, seed = NULL, sig.figs = 4, ledge_dist = 260, ledge_upper = 100, ledge_lower = 500)
{
  # for all those pesky "no visible binding" notes
  if(FALSE)
  {
    angle_mean <- angle_median <- angle_migration <- angle_sd <- ce <- ce_mean <- ce_median <- ce_sd <- NULL
    channel <- channel_a <- cross_at <- d <- delta_x <- delta_y <- directed_v_undirected <- distance_traveled <- NULL
    dvud <- dvud_p <- experiment <- finish_at <- finished <- Frame <- frames <- grp <- key_violation <- l <- max_y <- NULL
    max_v <- max_v_mean <- max_v_median <- max_v_sd <- minutes <- nchannels <- nsamps <- ntrts <- prop_finished <- NULL
    tot_finished <- Track <- treatment <- v <- v_x <- v_y <- X <- x <- Y <- y <- y_max <- y_min <- NULL
  }
  
  if(!is.null(seed))
    set.seed(seed)
  
  # make sure file structure inside of `results_dir` is correct
  # if(!file.exists(file.path(results_dir, 'images', experiment)))
  #   dir.create(file.path(results_dir, 'images', experiment), recursive = TRUE)
  
  # make sure we aren't getting more than 6 channels (indicates we have a day with multiple experiments that we missed)
  if(n_distinct(dat_sub$f) > 6)
    paste('More than 6 channels detected. This is likely due to multiple experiments being run on the same day. Please check the data and try again.',
          paste(unique(dat_sub$f), collapse = ', ')) |>
    stop()
  
  
  ##################################
  # Prep dat_sub for summarization #
  ##################################
  
  dat_sub <- dat_sub %>%
    
    unique() |> # remove duplicates - don't want them to be flagged as a key_violation below
    
    # should already be sorted, but just to be sure...
    arrange(experiment, channel, Track, Frame) %>%
    
    # look for cells that are in two places at once (this compares row i with row i+1)
    mutate(key_violation = c(Frame[-n()] == Frame[-1], FALSE) &
             c(Track[-n()] == Track[-1], FALSE)) %>%
  
    filter(!key_violation &                                      # matches the row after this
           !c(FALSE, key_violation[-length(key_violation)])) %>% # matches the row previous to this
  
    # split tracks that have large gaps in them (see if we still need this after upgrades to the tracking software)
    # split_tracks() %>%
    
    group_by(f, date, experiment, channel, sample, treatment, Track) %>%
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
             sum()
           ) |>
    ungroup() |>
    
    # add few tracks to drop criteria
    mutate(few_frames = pass_filters <= 3,                                                     # drop tracks with fewer than 3 frames
           little_movement = raw_distance < 10,                                                # drop tracks with less than 10 μm of movement
           drop = few_frames | little_movement | drop)
  

  ### count dropped records ###
  
  # tracks that didn't move
  non_movers <- dat_sub |>
    dplyr::select(channel, Track, non_mover) |>
    dplyr::distinct() |>
    group_by(channel) |>
    dplyr::summarize(non_movers = sum(non_mover)) |>
    ungroup()
  
  # tracks with very little movement (< 10 μm) - don't include tracks with few frames
  little_movement <- filter(dat_sub, !few_frames) |>
    dplyr::select(channel, Track, little_movement) |>
    dplyr::distinct() |>
    group_by(channel) |>
    dplyr::summarize(little_movement = sum(little_movement)) |>
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
  
  # tracks that have few observations
  few_frames <- dat_sub |>
    dplyr::select(channel, Track, few_frames) |>
    dplyr::distinct() |>
    group_by(channel) |>
    summarize(few_frames = sum(few_frames)) |>
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
    dplyr::select(-key_violation, -drop, -non_mover, -pre_start, -no_start, -post_end, -few_frames, -little_movement, -raw_distance)

  
  #############################
  # Track-level summarization #
  #############################

  track_summ <- dat_sub |>
    group_by(channel, Track, observe_finish) |>
    summarize(
      # calculate smooth functions of x and y over time
      x = map2(list(Frame), list(x), ~
                 {
                   tmp <- smooth.spline(.x, .y)
                   splinefun(tmp$x, tmp$y)
                 }),
      y = map2(list(Frame), list(y), ~
                 {
                   tmp <- smooth.spline(.x, .y)
                   splinefun(tmp$x, tmp$y)
                 }),
      frames = map(list(Frame), ~ unique(.x)),
      
      # calculate survival time
      # (finish_at and cross_at are in Frames, so divide by 2 for time in minutes)
      surv_time = unique(ifelse(observe_start, finish_at - cross_at, NA) / 2), # drop left censored tracks
      surv_event = unique(observe_finish)
    ) %>%
    ungroup() |>
    
    mutate(
      # calculate velocity over time (multiply by 2 to calculate velocity in micrometers  per minute - frames are every 30 seconds)
      v_x = map2(x, frames, function(x, f) x(f, deriv = 1) * 2),
      v_y = map2(y, frames, function(y, f) y(f, deriv = 1) * 2),
      v = map2(v_x, v_y, ~ sqrt(.x^2 + .y^2) * sign(.y)), # going down = positive velocity, going up = negative velocity
      
      # convert functions of x and y back to values
      x = map2(x, frames, function(x, f) x(f)),
      y = map2(y, frames, function(y, f) y(f)),
      
      # Chemotactic efficiency
      # calculate net change in y direction - return a value for each track (this could be negative if the cell travels in the wrong direction)
      delta_y = map_dbl(y, ~ .x[length(.x)] - .x[1]),
      
      # calculate the total distance traveled using the distance formula
      distance_traveled = map2_dbl(x, y, ~ sum( sqrt( (.x[-1]-.x[-length(.x)])^2+(.y[-1]-.y[-length(.y)])^2 ) )),
      
      # chemotactic efficiency for each track is the change in y (delta_y) divided by the total distance (distance_traveled) 
      ce = delta_y / distance_traveled,
      
      # Angle of migration
      # We need the total change in x direction to calculate the angle of migration
      delta_x = map_dbl(x, ~ abs(.x[length(.x)] - .x[1])),
      
      # this finds the angle of migration between the start and end point, converts from radians to degrees
      # zero degrees would represent a net movement straight in the vertical direction. angle is measured from this vertical line of 0 degrees
      angle_migration = 180*(abs(atan(delta_x/delta_y)))/pi,
      
      #peak velocity
      max_v = map_dbl(v, ~ max(.x)),
      
      #average velocity and standard deviation
      av_velocity = map_dbl(v, ~ mean(.x)),
      sd_velocity = map_dbl(v, ~ sd(.x))
    ) |>
  
    #deleting columns with intermediate variables (used for calculation but not needed in end file)
    select(-delta_y, -delta_x, -distance_traveled) |>
    
    # record the number of frames dropped from each track
    left_join(pre_start_frames, by = c("channel", "Track")) |>
    left_join(post_end_frames, by = c("channel", "Track")) |>
    mutate(pre_start_frames = ifelse(is.na(pre_start_frames), 0, pre_start_frames),
           post_end_frames  = ifelse(is.na( post_end_frames), 0,  post_end_frames))
  
  
  ###############################
  # Channel-level summarization #
  ###############################
  
  channel_summ <- group_by(dat_sub, f, date, experiment, channel, sample, treatment) %>%
    
    summarize(
      # calculate smooth functions of x and y over time
      x = map2(list(Frame), list(x), ~ 
                 {
                   tmp <- smooth.spline(.x, .y)
                   splinefun(tmp$x, tmp$y)
                 }),
      y = map2(list(Frame), list(y), ~ 
                 {
                   tmp <- smooth.spline(.x, .y)
                   splinefun(tmp$x, tmp$y)
                 }),
      frames = map(list(Frame), ~ unique(.x))) %>%
    ungroup() %>%
    
    mutate(
      # calculate velocity over time
      v_x = map2(x, frames, function(x, f) x(f, deriv = 1)),
      v_y = map2(y, frames, function(y, f) y(f, deriv = 1)),
      v = map2(v_x, v_y, ~ sqrt(.x^2 + .y^2) * sign(.y)), # going down = positive velocity, going up = negative velocity
      
      # convert functions of x and y back to values
      x = map2(x, frames, function(x, f) x(f)),
      y = map2(y, frames, function(y, f) y(f)),
      
      #directed vs undirected statistics for each channel
    ) %>%
    
    # add summary of track statistics (proportion finished, chemotactic efficiency, angle of migration)
    left_join({
      group_by(track_summ, experiment, channel) %>%
        summarize( tot_finished = sum(finished),
                  prop_finished = tot_finished / length(finished),
                  
                  ce_median = median(ce, na.rm = TRUE),
                  ce_mean   =   mean(ce, na.rm = TRUE),
                  ce_sd     =     sd(ce, na.rm = TRUE),

                  angle_median = median(angle_migration, na.rm = TRUE),
                  angle_mean   =   mean(angle_migration, na.rm = TRUE),
                  angle_sd     =     sd(angle_migration, na.rm = TRUE),

                  max_v_median = median(max_v, na.rm = TRUE),
                  max_v_mean   =   mean(max_v, na.rm = TRUE),
                  max_v_sd     =     sd(max_v, na.rm = TRUE)
        )}, by = c("experiment", "channel"))
  
  ##############################
  # Experiment-level summaries #
  ##############################
  
  exp_summ <- list()
  
  trts <- paste(unique(channel_summ$treatment))
  samps <- unique(channel_summ$sample)
  
  ### within_grp: Within-group statistics (within same sample, same treatment)
  within_grp <- group_by(dat_sub, date, experiment, sample, treatment) %>%
    
    # check to see how many channels we have in each group  
    mutate(nchannels = length(unique(na.omit(channel)))) %>%
    
    # drop any groups that only have one channel (nothing to compare)
    dplyr::filter(nchannels > 1)
  
  # if we have different within-group statistics to calculate...
  if(nrow(within_grp) > 0)
  {
    within_grp <- within_grp %>%
      
      # create one row per two-way comparison
      reframe(channel_a = combn(unique(na.omit(channel)), 2)[1,],
              channel_b = combn(unique(na.omit(channel)), 2)[2,],
              a_vs_b = list(''))
    
    # make comparisons for all pairs
    for(i in 1:nrow(within_grp))
    {
    }
  }else{
    within_grp <- tibble(date = as.Date(NA),
                         experiment = '',
                         sample = '',
                         treatment = '',
                         channel_a = -1,
                         channel_b = -1,
                         a_vs_b = list('')) %>%
      filter(channel_a != -1)
  }
  
  exp_summ$within_grp <- within_grp
  
  
  ### btw_trt: Between-treatment statistics (same sample, different treatment)
  btw_trt <- group_by(dat_sub, date, experiment, sample) %>%
    
    # check to see how many channels we have in each group  
    mutate(ntrts = length(unique(na.omit(treatment)))) %>%
    
    # drop any groups that only have one channel (nothing to compare)
    dplyr::filter(ntrts > 1,
                  !is.na(sample))
  
  # if we have different between-treatment statistics to calculate...
  if(nrow(btw_trt) > 0)
  {
    btw_trt <- btw_trt %>%
      
      # create one row per two-way comparison
      reframe(trt_a = paste(combn(unique(na.omit(treatment)), 2)[1,]),
              trt_b = paste(combn(unique(na.omit(treatment)), 2)[2,]),
              a_vs_b = list(''))
    
    # make comparisons for all pairs
    for(i in 1:nrow(btw_trt))
    {
    }
  }else{
    btw_trt <- tibble(date = as.Date(NA),
                      experiment = '',
                      sample = '',
                      trt_a = '',
                      trt_b = '',
                      a_vs_b = list('')) %>%
      filter(!is.na(date))
  }
  
  exp_summ$btw_trt <- btw_trt
  
  
  ### btw_samp: Between-sample comparison statistics (same treatment, different sample)
  btw_samp <- group_by(dat_sub, date, experiment, treatment) %>%
    
    # check to see how many channels we have in each group  
    mutate(nsamps = length(unique(na.omit(sample)))) %>%
    
    # drop any groups that only have one channel (nothing to compare)
    dplyr::filter(nsamps > 1)
  
  # if we have different between-treatment statistics to calculate...
  if(nrow(btw_samp) > 0)
  {
    btw_samp <- btw_samp %>%
      
      # create one row per two-way comparison
      reframe(a = paste(combn(unique(na.omit(sample)), 2)[1,]),
              b = paste(combn(unique(na.omit(sample)), 2)[2,]),
              a_vs_b = list(''))
    
    # make comparisons for all pairs
    for(i in 1:nrow(btw_samp))
    {
    }
  }else{
    btw_samp <- tibble(date = as.Date(NA),
                       experiment = '',
                       treatment = '',
                       trt_a = '',
                       trt_b = '',
                       a = '',
                       b = '',
                       a_vs_b = list('')) %>%
      filter(!is.na(date))
  }
  
  exp_summ$btw_samp <- btw_samp
  
  
  ###########
  # Figures #
  ###########
  
  expSummary <- tibble(expID = experiment,
                       tracks_time = file.path('images', experiment, 'tracks_time'),
                       tracks_v = file.path('images', experiment, 'tracks_v'),
                       angle_migration = file.path('images', experiment, 'angle_migration'),
                       ce = file.path('images', experiment, 'ce'))
    
  ##### Return Results #####
  
  list(expSummary = expSummary,
       
       expStats = tibble(expID   = experiment,
                         within  = c(btw_samp$treatment, btw_trt$sample),
                         between = c(paste(btw_samp$a,    btw_samp$b,     sep = ', '),
                                     paste( btw_trt$trt_a, btw_trt$trt_b, sep = ', ')),
                         test    = 'dissim',
                         stat    = c(map_dbl(btw_samp$a_vs_b, ~ .x['dissim']),
                                     map_dbl( btw_trt$a_vs_b, ~ .x['dissim'])),
                         p       = c(map_dbl(btw_samp$a_vs_b, ~ .x['p']),
                                     map_dbl( btw_trt$a_vs_b, ~ .x['p']))),
       
       chanSummary = select(channel_summ, 
                            experiment, channel, sample, treatment, 
                            tot_finished, prop_finished, 
                            ce_median, ce_mean, ce_sd,
                            angle_median, angle_mean, angle_sd,
                            max_v_median, max_v_mean, max_v_sd,
                            dvud, dvud_p) %>%
         rename(expID  = experiment, 
                chanID = channel,
                sID    = sample),
       
       chanRaw = map_df(1:nrow(channel_summ), ~ tibble(expID  = channel_summ$experiment[.x],
                                                       chanID = channel_summ$channel[.x],
                                                       x      = channel_summ$x[[.x]],
                                                       y      = channel_summ$y[[.x]],
                                                       frames = channel_summ$frames[[.x]] %>% as.integer(),
                                                       v_x    = channel_summ$v_x[[.x]],
                                                       v_y    = channel_summ$v_y[[.x]],
                                                       v      = channel_summ$v[[.x]])),
       
       trackSummary = tibble(expID           = track_summ$experiment,
                             chanID          = track_summ$channel,
                             trackID         = track_summ$Track %>% as.integer(),
                             ce              = track_summ$ce,
                             angle_migration = track_summ$angle_migration,
                             max_v           = track_summ$max_v,
                             av_velocity     = track_summ$av_velocity,
                             finished        = track_summ$finished),
       
       trackRaw = map_df(1:nrow(track_summ), ~ tibble(expID   = track_summ$experiment[.x],
                                                      chanID  = track_summ$channel[.x],
                                                      trackID = track_summ$Track[.x] %>% as.integer()) |>
                           
                           left_join(select(dat_sub, experiment, channel, Track, X, Y), # add X,Y position data back in (pixel coordinates)
                                     by = c('expID' = 'experiment', 'chanID' = 'channel', 'trackID' = 'Track')) |>
                           rename(x_px = X, y_px = Y) |>
                           
                           mutate(x       = track_summ$x[[.x]],
                                  y       = track_summ$y[[.x]],
                                  frames  = track_summ$frames[[.x]] %>% as.integer(),
                                  v_x     = track_summ$v_x[[.x]],
                                  v_y     = track_summ$v_y[[.x]],
                                  v       = track_summ$v[[.x]]))
       )
}
