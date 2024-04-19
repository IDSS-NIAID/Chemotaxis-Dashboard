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
#' @param ledge_dist Numeric, distance between top and bottom ledge of the microscope image in micrometers (default is 260)
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
#' @importFrom readr read_delim
process_experiments <- function(experiment, source_dir, results_dir, seed = NULL, sig.figs = 4, ledge_dist = 260)
{
  # for all those pesky "no visible binding" notes
  if(FALSE)
    bad <- X <- Y <- NULL
  
  options(dplyr.summarise.inform = FALSE)

  # source files for processing
  f <- list.files(source_dir)
  f <- map(experiment, ~ grep(.x, f, value = TRUE)) %>%
    unlist()
  

  ##### experiment metadata #####
  results_meta <- tibble(
    f = f,

    dat = strsplit(f, '_', fixed = TRUE),

    date = {map_chr(dat, `[`, 1) %>%
        substr(1, 8) %>%
        as.Date(format = '%Y%m%d')},
    
    bad = map_lgl(dat, ~ grepl(.x, pattern = '^[0-9]$') %>% any()), # some dates have two experiments - this will catch those

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

    mutate(sample = tolower(sample)) %>% # inconsistent capitalization

    dplyr::select(-dat)


  ##### read in raw data #####
  dat <- map2_df(source_dir, f, ~
                   {
                     paste(.x, .y, sep = '/') %>%
                       read_delim(delim = '\t', col_types = 'dddd') %>%
                       mutate(f = .y)
                   }) %>%

    bind_rows() %>%

    left_join(results_meta, by = 'f') %>%

    # pull experiment name
    mutate(experiment = map2_chr(f, ifelse(bad, 10, 8), ~ str_sub(.x, start = 1, end = .y))) %>%

    filter(!is.na(X) & !is.na(Y)) %>%
    
    select(-bad)

  
  ##### pre-process data for these experiments #####
  set.seed(seed)
  
  retval <- unique(dat$experiment) %>%
    map(~ one_experiment(dat_sub = filter(dat, experiment == .x),
                         experiment = .x,
                         results_dir = results_dir, 
                         sig.figs = sig.figs,
                         ledge_dist = ledge_dist))

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
#' @param sig.figs maximum significant digits for p-values obtained by permutation testing
#' @param ledge_dist Numeric, distance between top and bottom ledge of the microscope image in micrometers (default is 260)
#' 
#' @return A list of data.frames containing summaries and raw data from the data in dat_sub.
#' @export
#' @import rlang
#' @importFrom magrittr %>%
#' 
#' @importFrom dplyr arrange case_when filter group_by left_join mutate reframe rename select starts_with summarize ungroup
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map map_dbl map_lgl map2 map2_dbl pmap
#' @importFrom stats lm median quantile sd smooth.spline splinefun
#' @importFrom splines bs
#' @importFrom utils combn
#' @importFrom ggplot2 aes ggplot element_blank facet_wrap geom_boxplot geom_jitter geom_hline geom_path geom_violin ggsave scale_y_reverse scale_color_gradient2 scale_color_manual stat_smooth theme theme_set xlab ylab
#' @importFrom cowplot theme_cowplot
#' @importFrom grDevices rgb
one_experiment <- function(dat_sub, experiment, results_dir, sig.figs = 4, ledge_dist = 260)
{
  # for all those pesky "no visible binding" notes
  if(FALSE)
  {
    angle_mean <- angle_median <- angle_migration <- angle_sd <- ce <- ce_mean <- ce_median <- ce_sd <- NULL
    channel <- channel_a <- cross_at <- d <- delta_x <- delta_y <- directed_v_undirected <- distance_traveled <- NULL
    dvud <- dvud_p <- experiment <- finished <- Frame <- frames <- grp <- l <- max_y <- max_v <- max_v_mean <- NULL
    max_v_median <- max_v_sd <- minutes <- nchannels <- nsamps <- ntrts <- prop_finished <- tot_finished <- NULL
    Track <- treatment <- v <- v_x <- v_y <- X <- x <- Y <- y <- y_max <- y_min <- NULL
  }
  
  # make sure file structure inside of `results_dir` is correct
  if(!file.exists(file.path(results_dir, 'images', experiment)))
    dir.create(file.path(results_dir, 'images', experiment), recursive = TRUE)
  
  
  ##################################
  # Prep dat_sub for summarization #
  ##################################
  
  dat_sub <- dat_sub %>%
    
    # split tracks that have large gaps in them (see if we still need this after upgrades to the tracking software)
    # split_tracks() %>%
    
    mutate(
      # this is the frame where the cell first crosses the upper ledge
      cross_at = case_when(    Y[1] >= 0  ~ Frame[1],
                               all(Y    <  0) ~ Frame[n()], # if it never crosses the top ledge, use the last frame as the starting point
                               TRUE           ~ suppressWarnings(min(Frame[c(FALSE, Y[-length(Y)] < 0 & Y[-1] >= 0)], na.rm = TRUE))),
      
      # translate X st each cell starts at (0,~0) when first crossing top ledge
      # x = X - X[Frame == cross_at],
      
      # X and Y are scaled s.t. the distance from the top to the bottom ledge is 1
      # convert to micrometers
      x = X * ledge_dist,
      y = Y * ledge_dist,
      
      y_min = min(y),
      y_max = max(y),
      
      # remove '.csv' from file names
      f = gsub('.csv', '', f, fixed = TRUE)) %>%
    ungroup() %>%
    
    # drop any "cells" that don't move at all (+/- a few pixels)
    filter(y_max - y_min > 0.01 &
             # drop this one that didn't work
             (is.na(treatment) | treatment != 'fMLF (did not work)'))
  
  
  #############################
  # Track-level summarization #
  #############################
  
  track_summ <- group_by(dat_sub, channel, sample, treatment, Track, experiment) %>%
    
    # make sure we have enough observations to use the track
    mutate(l = sum(!is.na(x))) %>%
    filter(l > 3) %>%
    
    # calculate smooth functions of x and y over time
    summarize(
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
      
      #average velocity
      av_velocity = map_dbl(v, ~ mean(.x)),
      
      # Proportion of cells making it past the threshold
      # at the track level, we want to know if the cell ever passes the bottom ledge
      # to understand that, we set a variable "finished" to be TRUE if the cell crosses the bottom ledge and 0 if it does not
      max_y = map_dbl(y, ~ max(.x)),
      finished = max_y >= ledge_dist)
  
  #deleting columns with intermediate variables (used for calculation but not needed in end file)
  track_summ <- select(track_summ, -delta_y, -delta_x, -distance_traveled, -max_y)
  
  
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
      directed_v_undirected = pmap(list(chan = channel, frames = frames, f = x, g = y),
                                   function(chan, frames, f, g)
                                     filter(dat_sub, channel == chan) %>%
                                     dplyr::select(Track, Frame, x, y) %>% 
                                     rename(f = x, g = y) %>%
                                     compare_two_functions(frames, f, g, sig.figs)),
      dvud   = map_dbl(directed_v_undirected, ~ .x['dissim']),
      dvud_p = map_dbl(directed_v_undirected, ~ .x['p']),
    ) %>%
    
    # add summary of track statistics (proportion finished, chemotactic efficiency, angle of migration)
    left_join({
      group_by(track_summ, experiment, channel) %>%
        summarize(tot_finished = length(finished),
                  prop_finished = sum(finished) / tot_finished,
                  
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
      # get functions to compare
      f <- filter(dat_sub, paste(treatment) == paste(within_grp$treatment[i]) &
                    paste(sample   ) == paste(within_grp$sample[i]) &
                    channel    == within_grp$channel_a[i])
      f <- list(smooth.spline(f$Frame, f$x),
                smooth.spline(f$Frame, f$y))
      
      g <- filter(dat_sub, paste(treatment) == paste(within_grp$treatment[i]) &
                    paste(sample   ) == paste(within_grp$sample[i]) &
                    channel    ==       within_grp$channel_b[i])
      g <- list(smooth.spline(g$Frame, g$x),
                smooth.spline(g$Frame, g$y))
      
      # calculate similarity and p-value
      within_grp$a_vs_b[[i]] <- filter(dat_sub, paste(treatment) == paste(within_grp$treatment[i]) &
                                         paste(sample   ) == paste(within_grp$sample[i]) &
                                         channel   %in%    c(within_grp$channel_a[i], within_grp$channel_b[i])) %>%
        mutate(f = x) %>%
        compare_two_functions(f = sapply(f, function(.x) .x$y), frames.f = f[[1]]$x, 
                              g = sapply(g, function(.x) .x$y), frames.g = g[[1]]$x,
                              sig.figs = sig.figs, lab = 'channel')
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
      # get functions to compare
      f <- filter(dat_sub, paste(treatment) == paste(btw_trt$trt_a[i]) &
                                 sample     ==       btw_trt$sample[i])
      f <- list(smooth.spline(f$Frame, f$x),
                smooth.spline(f$Frame, f$y))
      
      g <- filter(dat_sub, paste(treatment) == paste(btw_trt$trt_b[i]) &
                                 sample     ==       btw_trt$sample[i])
      g <- list(smooth.spline(g$Frame, g$x),
                smooth.spline(g$Frame, g$y))
      
      # calculate similarity and p-value
      btw_trt$a_vs_b[[i]] <- filter(dat_sub, paste(treatment) %in% c(paste(btw_trt$trt_a[i]), paste(btw_trt$trt_b[i])) &
                                      sample == btw_trt$sample[i]) %>%
        mutate(f = x) %>%
        compare_two_functions(f = sapply(f, function(.x) .x$y), frames.f = f[[1]]$x, 
                              g = sapply(g, function(.x) .x$y), frames.g = g[[1]]$x,
                              sig.figs = sig.figs, lab = 'treatment')
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
      # get functions to compare
      f <- filter(dat_sub, paste(sample   ) == paste(btw_samp$a[i]) &
                    paste(treatment) == paste(btw_samp$treatment[i]))
      f <- list(smooth.spline(f$Frame, f$x),
                smooth.spline(f$Frame, f$y))
      
      g <- filter(dat_sub, paste(sample   ) == paste(btw_samp$b[i]) &
                    paste(treatment) == paste(btw_samp$treatment[i]))
      g <- list(smooth.spline(g$Frame, g$x),
                smooth.spline(g$Frame, g$y))
      
      # calculate similarity and p-value
      btw_samp$a_vs_b[[i]] <- filter(dat_sub, paste(sample  ) %in% c(paste(btw_samp$a[i]), paste(btw_samp$b[i])) &
                                       paste(treatment) ==    paste(btw_samp$treatment[i])) %>%
        mutate(f = x) %>%
        compare_two_functions(f = sapply(f, function(.x) .x$y), frames.f = f[[1]]$x, 
                              g = sapply(g, function(.x) .x$y), frames.g = g[[1]]$x,
                              sig.figs = sig.figs, lab = 'sample')
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
  
  
  ### tracks_time: time-coded tracks
  tracks_time <- arrange(dat_sub, channel, Track, Frame) %>%
    
    mutate(lab = paste0(channel, ": ", sample, ", ", treatment),
           minutes = Frame / 2) %>%
    
    ggplot(aes(x = x, y = y, group = Track, color = minutes)) +
    geom_path() + #connects observations in the order in which they appear in the dataset
    
    ylab('Directed Movement') +
    xlab('Non-directed Movement') +
    
    facet_wrap(~ lab) + #produces multi-panel plot, separate by 'lab'
    
    scale_y_reverse() +
    scale_color_gradient2(low  = 'blue',
                          mid  = rgb(  0, .62, .45),
                          high = rgb(.9, .62, 0),
                          midpoint = 30) +
    
    geom_hline(yintercept = 0, linetype = 2) + #draws a horizontal line at y = 0
    geom_hline(yintercept = 1, linetype = 2) #draws a horizontal line at y = 1
  
  save(tracks_time,
       file = file.path(results_dir, paste0(expSummary$tracks_time, '.RData')))
  
  ggsave(file.path(results_dir, paste0(expSummary$tracks_time, '.png')),
         tracks_time, width = 10, height = 6)
  
  
  ### tracks_v: velocity over time graphs
  tracks_v <-  pivot_longer(track_summ, starts_with('v'), names_to = 'd', values_to = 'v') %>%
    filter(d != 'v') %>% #filters out first row (I think)
    #d = direction, pivot_longer makes data frame longer by making a column 'd' for direction (x-undirected, y-directed)
    
    # split out velocity curves for each track
    group_by(channel, Track, sample, treatment, d) %>% #making data frame longer allows us to split by directed and undirected (d)
    reframe(v = unlist(v), #list -> vector
            Frame = unlist(frames)) %>%
    mutate(grp = paste(channel, d)) %>% 
    
    
    # join channels that have the same sample and treatment
    group_by(sample, treatment, d) %>%
    mutate(joint_channels = paste0(paste(unique(channel), collapse = '/'), ": ", unique(sample), ', ', unique(treatment))) %>%
    ungroup() %>%
    
    # sort and plot
    arrange(channel, d, Track, Frame) %>%
    mutate(minutes = Frame / 2) %>%
    
    
    #plots velocity (v) over time (Frame), grouping the data by grp which is channel and direction, coloring them by direction  
    ggplot(aes(minutes, v, group = grp, color = d)) +
    
    stat_smooth(method = lm, formula = y ~ bs(x, df = 3), se = FALSE) +
    stat_smooth(method = lm, formula = y ~ 1, se = FALSE, linetype = 2, linewidth = .5) +
    
    scale_color_manual(values = c('black', 'gold3'), labels = c('Undirected', 'Directed')) +
    theme(legend.title = element_blank(),
          legend.position = 'top') +
    ylab(expression(paste('Relative Velocity (', mu, 'm / min)'))) +
    geom_hline(yintercept = 0, linetype = 3, linewidth = .5)
  
  #if sample and treatment are available, split by these labels; if not, use channels
  if(!(any(is.na(track_summ$sample)))){
    tracks_v <- tracks_v + facet_wrap(~joint_channels)
  }else{
    tracks_v <- tracks_v + facet_wrap(~channel)
  }
  
  save(tracks_v,
       file = file.path(results_dir, paste0(expSummary$tracks_v, '.RData')))
  
  ggsave(file.path(results_dir, paste0(expSummary$tracks_v, '.png')),
         tracks_v, width = 10, height = 6)
  
  
  ### angle of migration
  angle_migration_plot <- track_summ %>% 
    group_by(channel, sample, treatment) %>% 
    
    ggplot(aes(x=as.character(channel), y=angle_migration, group=channel)) +
    
    geom_violin(scale = "width", width = 0.5, trim=FALSE) + 
    geom_jitter(width = 0.1,size = 0.75,alpha=0.3) + 
    geom_boxplot(width = 0.05) +
    
    ylab("Migration angle (degrees from vertical)") + 
    xlab("Channel")
  
  save(angle_migration_plot,
       file = file.path(results_dir, paste0(expSummary$angle_migration, '.RData')))
  
  ggsave(file.path(results_dir, paste0(expSummary$angle_migration, '.png')),
         angle_migration_plot, width = 10, height = 6)
  
  
  ### chemotactic efficiency
  ce_plot <- track_summ %>% 
    group_by(channel, sample, treatment) %>% 
    
    ggplot(aes(x=as.character(channel), y=ce, group=channel)) +
    
    geom_violin(scale = "width", width = 0.5, trim=FALSE) + 
    geom_jitter(width = 0.1, size = 0.75, alpha=0.3) + 
    geom_boxplot(width = 0.05) +
    
    ylab("Chemotactic efficiency (% vertical movement)") + 
    xlab("Channel")
  
  save(ce_plot,
       file = file.path(results_dir, paste0(expSummary$ce, '.RData')))
  
  ggsave(file.path(results_dir, paste0(expSummary$ce, '.png')),
         ce_plot, width = 10, height = 6)

    
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
                                                       y      = channel_summ$x[[.x]],
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
                                                      trackID = track_summ$Track[.x] %>% as.integer(),
                                                      x       = track_summ$x[[.x]],
                                                      y       = track_summ$x[[.x]],
                                                      frames  = track_summ$frames[[.x]] %>% as.integer(),
                                                      v_x     = track_summ$v_x[[.x]],
                                                      v_y     = track_summ$v_y[[.x]],
                                                      v       = track_summ$v[[.x]])))
}


#' Functional data analysis comparison of two functions, f1 and f2
#' 
#' @param data A subset of dat_sub with variables, f and g, for permutation testing. When `lab` is specified, only `f` is used.
#' @param frames.f A vector of frames over which the function, f, is defined
#' @param f A vector or matrix of responses for f
#' @param g A vector or matrix of responses for g
#' @param sig.figs Integer, maximum significant digits for p-values obtained by permutation testing
#' @param frames.g A vector of frames over which the function, g, is defined
#' @param lab A character string indicating which variable is to be shuffled in the permutations. If NULL, then it is assumed that the two variables to be compared are `f` and `g`.
#' 
#' @details There are two main uses for this function: to compare two functions defined for each track (i.e. directed vs
#' undirected travel, where we shuffle un/directed values for each individual track) and to compare a single function 
#' between two different groups of tracks.
#' 
#' 1) When comparing two functions defined for each track, we assume that there are two columns of data, labeled `f` 
#' and `g`. The will be compared, and in the permutation stage, they will be randomly swapped for about half of the 
#' Tracks in each permutation.
#' 
#' 2) When comparing a single between two different groups of tracks, we assume that there is one column of data labeled,
#' `f`, and another column identified by `lab`. During the permutation stage, a group of tracks will be randomly assigned
#' group labels, maintaining the total number of Tracks in each group. Exactly two groups should be included in the data
#' set.
#' 
#' @return A named vector containing the similarity measure and permutation p-value
#' .data <- filter(dat_sub, channel == 1) %>% dplyr::select(Track, Frame, x, y) %>% rename(f = x, g = y)
#' frames <- channel_summ$frames[[1]]
#' f <- channel_summ$x[[1]]
#' g <- channel_summ$y[[1]]
#' 
#' @export
#' @import rlang
#' @importFrom magrittr %>%
#' @importFrom dplyr matches select mutate
#' @importFrom foreach foreach %dopar%
#' @importFrom stats na.omit rbinom smooth.spline
#' @importFrom briKmeans kma.similarity
compare_two_functions <- function(data, frames.f, f, g, sig.figs, frames.g = frames.f, lab = NULL)
{
  # for all those pesky "no visible binding" notes
  if(FALSE)
    f.tmp <- perm_lab <- Track <- NULL
  
  if(is.null(lab))
  {
    # tracks for permutation test
    tracks <- unique(data$Track)
  }else{
    # if we are using a label, tabulate how many tracks fall into each group
    grps <- dplyr::select(data, Track, matches(lab)) %>%
      unique() %>%
      dplyr::select(-Track) %>%
      unlist() %>%
      paste() %>% # need this to catch 'NA' as an option
      table()
    
    # labels for permutation test
    data$perm_lab <- paste(data[[lab]], data$Track)
    tracks <- unique(data$perm_lab)
    
    # for some reason, we need to do this - from the documentation, it seems like we shouldn't, but...
    frames.f <- cbind(frames.f, frames.f)
    frames.g <- cbind(frames.g, frames.g)
  }
  
  # similarity measure
  dissim <- suppressWarnings(kma.similarity(x.f = frames.f, y0.f = f,
                                            x.g = frames.g, y0.g = g, 
                                            similarity.method = 'd0.L2'))
  
  # permutation test
  # shuffles labels on data then generates distribution of test statistic for each shuffle
  # then calculates p-value for actual data's statistic as compared to the simulated test statistic
  # will shuffle ~10^i times (min i = 2 = 100 shuffles)
  perms <- numeric(0)
  for(i in 2:max(2, sig.figs))
  {
    
    perms <- foreach(i = 1:1*10^i,
                     .combine = 'c') %dopar%
      {
        # shuffle f and g
        if(is.null(lab))
        {
          # pick tracks to shuffle
          pick_track <- as.logical(rbinom(length(tracks), 1, 0.5))
          
          shuffled <- mutate(data, 
                             f.tmp = ifelse(Track %in% tracks[pick_track], g, f),
                             g     = ifelse(Track %in% tracks[pick_track], f, g),
                             f     = f.tmp)
          
          # smooth f and g
          f_shuff <- smooth.spline(shuffled$Frame, shuffled$f)
          g_shuff <- smooth.spline(shuffled$Frame, shuffled$g)
        }else{
          # pick tracks to be in group 1, the others fall into group 2 (should exactly two groups)
          grp1 <- sample(tracks, grps[1])
          
          # shuffle
          shuffled <- mutate(data,
                             lab = ifelse(perm_lab %in% grp1, names(grps)[1], names(grps)[2]))
          
          # smooth f and g
          tmp <- dplyr::filter(shuffled, lab == names(grps)[1])  # filter by shuffled group
          f_shuff <- list(smooth.spline(tmp$Frame, tmp$x),             # smooth undirected function
                          smooth.spline(tmp$Frame, tmp$y))             # smooth directed function
          
          f_shuff <- list(x = sapply(f_shuff, function(.x) .x$x),
                          y = sapply(f_shuff, function(.x) .x$y))      # return two columns (un/directed) of smoothed functions
          
          tmp <- dplyr::filter(shuffled, lab == names(grps)[2])  # filter by shuffled group
          g_shuff <- list(smooth.spline(tmp$Frame, tmp$x),             # smooth undirected function
                          smooth.spline(tmp$Frame, tmp$y))             # smooth directed function
          
          g_shuff <- list(x = sapply(g_shuff, function(.x) .x$x),
                          y = sapply(g_shuff, function(.x) .x$y))      # return two columns (un/directed) of smoothed functions
        }
        
        # calculate similarity
        suppressWarnings(kma.similarity(x.f = f_shuff$x, y0.f = f_shuff$y, 
                                        x.g = g_shuff$x, y0.g = g_shuff$y,
                                        similarity.method = 'd0.L2'))
      } %>%
      
      c(perms)
    
    # return permutation test p-value, null hypothesis is that f and g are the same
    retval <- c(dissim = dissim,
                p = sum(dissim < perms) / length(perms))
    
    # don't do extra simulations if it looks like the p-value is too large
    if(retval['p'] > 1*10^-(i - 1))
      break
  }
  
  return(retval)
}