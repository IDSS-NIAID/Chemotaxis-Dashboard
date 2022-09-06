# one_experiment.R
# process data from one experiment and save

#' function to display "Too much missing information" message in place of a regular figure
#' 
#' @return A ggplot2 figure
#'
#' @import rlang
#' @importFrom magrittr %>%
#' 
#' @importFrom tibble tibble
#' 
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
missing_info <- function()
{
    tibble(x = 1, y = 1, label = 'Too much missing information') %>%
        ggplot(aes(x = .data$x, y = .data$y, label = .data$label)) +
        geom_text() +
        ylab('') +
        xlab('') +
        theme(axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.line = element_blank()) %>%
        return()
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
#' .data <- filter(dat_sub, channel == 1) %>% dplyr::select(Track, Frame, X, Y) %>% rename(f = X, g = Y)
#' frames <- channel_summ$frames[[1]]
#' f <- channel_summ$x[[1]]
#' g <- channel_summ$y[[1]]
#' 
#' @import rlang
#' @importFrom magrittr %>%
#'
#' @importFrom dplyr matches
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' 
#' @importFrom stats rbinom
#' @importFrom stats smooth.spline
#' 
#' @importFrom fdakma kma.similarity
compare_two_functions <- function(data, frames.f, f, g, sig.figs, frames.g = frames.f, lab = NULL)
{

  if(is.null(lab))
  {
    # tracks for permutation test
    tracks <- unique(data$Track)
  }else{
    # if we are using a label, tabulate how many tracks fall into each group
    grps <- dplyr::select(data, .data$Track, matches(lab)) %>%
      unique() %>%
      dplyr::select(-.data$Track) %>%
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
  for(i in 2:max(2, sig.figs))
  {
  
    perms <- replicate(1*10^i, 
    {
      # shuffle f and g
      if(is.null(lab))
      {
        # pick tracks to shuffle
        pick_track <- as.logical(rbinom(length(tracks), 1, 0.5))
  
        shuffled <- mutate(data, 
                           f.tmp = ifelse(.data$Track %in% tracks[pick_track], g, f),
                           g     = ifelse(.data$Track %in% tracks[pick_track], f, g),
                           f     = .data$f.tmp)
            
        # smooth f and g
        f_shuff <- smooth.spline(shuffled$Frame, shuffled$f)
        g_shuff <- smooth.spline(shuffled$Frame, shuffled$g)
      }else{
        # pick tracks to be in group 1, the others fall into group 2 (should exactly two groups)
        grp1 <- sample(tracks, grps[1])
          
        # shuffle
        shuffled <- mutate(data,
                           lab = ifelse(.data$perm_lab %in% grp1, names(grps)[1], names(grps)[2]))
                             
        # smooth f and g
        shuffled <- dplyr::filter(shuffled, .data$lab == names(grps)[1])  # filter by shuffled group
        f_shuff <- list(smooth.spline(shuffled$Frame, shuffled$X),        # smooth undirected function
                        smooth.spline(shuffled$Frame, shuffled$Y))        # smooth directed function
      
        f_shuff <- list(x = sapply(f_shuff, function(.x) .x$x),
                        y = sapply(f_shuff, function(.x) .x$y))           # return two columns (un/directed) of smoothed functions
        
        shuffled <- dplyr::filter(shuffled, .data$lab == names(grps)[2])  # filter by shuffled group
        g_shuff <- list(smooth.spline(shuffled$Frame, shuffled$X),        # smooth undirected function
                        smooth.spline(shuffled$Frame, shuffled$Y))        # smooth directed function
      
        g_shuff <- list(x = sapply(g_shuff, function(.x) .x$x),
                        y = sapply(g_shuff, function(.x) .x$y))           # return two columns (un/directed) of smoothed functions
      }
        
      # calculate similarity
      suppressWarnings(kma.similarity(x.f = f_shuff$x, y0.f = f_shuff$y, 
                                      x.g = g_shuff$x, y0.g = g_shuff$y,
                                      similarity.method = 'd0.L2'))
    })
    
    # return permutation test p-value, null hypothesis is that f and g are the same
    retval <- c(dissim = dissim,
                p = sum(dissim < perms) / (1*10^i))
    
    # don't do extra simulations if it looks like the p-value is too large
    if(retval['p'] > 1*10^-(i - 1))
      break
  }
  
  return(retval)
}

#' Analysis of data for each experiment
#' 
#' @param dat_sub data frame (tibble) containing the subset of data for a single experiment
#' @param sig.figs maximum significant digits for p-values obtained by permutation testing
#' @param root path to the root directory of this repository (or wherever else you want to be saving data - will save to `root/data/.`)
#' 
#' @return A data frame containing channel-level summaries of the data in dat_sub. A list containing experiment-level statistics is also saved to an RData file.
#' @export
#' @import rlang
#' @importFrom magrittr %>%
#' 
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr case_when
#' @importFrom dplyr summarize
#' @importFrom dplyr arrange
#' @importFrom dplyr left_join
#' @importFrom dplyr starts_with
#' 
#' @importFrom tidyr pivot_longer
#' 
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom purrr map_dbl
#' @importFrom purrr map2_dbl
#' @importFrom purrr map_lgl
#' @importFrom purrr pmap
#' 
#' @importFrom stats quantile
#' @importFrom stats median
#' @importFrom stats lm
#' @importFrom stats sd
#' 
#' @importFrom utils combn
#' 
#' @importFrom ggplot2 geom_path
#' @importFrom ggplot2 scale_y_reverse
#' @importFrom ggplot2 scale_color_gradient2
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 geom_violin
#' @importFrom ggplot2 geom_jitter
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 stat_smooth
one_experiment <- function(dat_sub, root = '', sig.figs = 4)
{
    ##################################
    # Prep dat_sub for summarization #
    ##################################
    
    dat_sub <- group_by(dat_sub, .data$f, .data$date, .data$experiment, .data$channel, .data$sample, .data$treatment, .data$Track) %>%
        mutate(
            # this is the frame where the cell first crosses the upper ledge
            cross_at = case_when(    .data$Y[1] >= 0  ~ .data$Frame[1],
                                 all(.data$Y    <  0) ~ .data$Frame[1], # if it never crosses the top ledge, use time 0 as the starting point
                                     TRUE           ~ suppressWarnings(min(.data$Frame[c(FALSE, .data$Y[-length(.data$Y)] < 0 & .data$Y[-1] >= 0)], na.rm = TRUE))),
        
            # X and Y are already scaled - translate X st each cell starts at (0,~0) when first crossing top ledge
            X = .data$X - .data$X[.data$Frame == .data$cross_at],
            
            y_min = min(.data$Y),
            y_max = max(.data$Y)) %>%
        ungroup() %>%
        
        # drop any "cells" that don't move at all (+/- a few pixels)
        filter(.data$y_max - .data$y_min > 0.01 &
               # drop this one that didn't work
               (is.na(.data$treatment) | .data$treatment != 'fMLF (did not work)')) %>%
        
        # remove '.csv' from file names
        mutate(f = gsub('.csv', '', .data$f, fixed = TRUE))
    
    
    #############################
    # Track-level summarization #
    #############################
    
    track_summ <- group_by(dat_sub, .data$channel, .data$sample, .data$treatment, .data$Track, .data$experiment) %>%
        
        # make sure we have enough observations to use the track
        mutate(l = sum(!is.na(.data$X))) %>%
        filter(.data$l > 3) %>%
        
        # calculate smooth functions of X and Y over time
        summarize(
            x = map2(list(.data$Frame), list(.data$X), ~ 
                    {
                      tmp <- smooth.spline(.x, .y)
                      splinefun(tmp$x, tmp$y)
                    }),
            y = map2(list(.data$Frame), list(.data$Y), ~ 
                    {
                      tmp <- smooth.spline(.x, .y)
                      splinefun(tmp$x, tmp$y)
                    }),
            frames = map(list(.data$Frame), ~ unique(.x))) %>%
        ungroup() %>%
        
        mutate(
            # calculate velocity over time
            v_x = map2(.data$x, .data$frames, function(x, f) x(f, deriv = 1)),
            v_y = map2(.data$y, .data$frames, function(y, f) y(f, deriv = 1)),
            v = map2(.data$v_x, .data$v_y, ~ sqrt(.x^2 + .y^2) * sign(.y)), # going down = positive velocity, going up = negative velocity
        
            # convert functions of x and y back to values
            x = map2(.data$x, .data$frames, function(x, f) x(f)),
            y = map2(.data$y, .data$frames, function(y, f) y(f)),
            
            # Chemotactic efficiency
            # calculate total change in y direction - return a value for each track (this could be negative if the cell travels in the wrong direction)
            delta_y = map_dbl(.data$y, ~ .x[length(.x)] - .x[1]),
            
            # calculate the total distance traveled using the distance formula
            distance_traveled = map2_dbl(.data$x, .data$y, ~ sum( sqrt( (.x[-1]-.x[-length(.x)])^2+(.y[-1]-.y[-length(.y)])^2 ) )),
            
            # chemotactic efficiency for each track is the change in Y (delta_y) divided by the total distance (distance_traveled) 
            ce = .data$delta_y / .data$distance_traveled,
            
            # Angle of migration
            # We need the total change in x direction to calculate the angle of migration
            delta_x = map_dbl(.data$x, ~ abs(.x[length(.x)] - .x[1])),
            
            # this finds the angle of migration between the start and end point, converts from radians to degrees
            # zero degrees would represent a net movement straight in the vertical direction. angle is measured from this vertical line of 0 degrees
            angle_migration = 180*(abs(atan(.data$delta_x/.data$delta_y)))/pi,

            #peak velocity
            max_v = map_dbl(.data$v, ~ max(.x)),

            #average velocity
            av_velocity = map_dbl(.data$v, ~ mean(.x)),
                                    
            # Proportion of cells making it past the threshold
            # at the track level, we want to know if the cell ever passes the y-position 1
            # to understand that, we set a variable "finished" to be 1 if the cell crosses the threshold and 0 if it does not
            max_y = map_dbl(.data$y, ~ max(.x)),
            finished = ifelse(.data$max_y >= 1, 1, 0))
    
    #deleting columns with intermediate variables (used for calculation but not needed in end file)
    track_summ <- select(track_summ, -.data$delta_y, -.data$delta_x, -.data$distance_traveled, -.data$max_y)
    
  
    
    ###############################
    # Channel-level summarization #
    ###############################
    
    channel_summ <- group_by(dat_sub, .data$f, .data$date, .data$experiment, .data$channel, .data$sample, .data$treatment) %>%
        
        summarize(
            # calculate smooth functions of X and Y over time
            x = map2(list(.data$Frame), list(.data$X), ~ 
                    {
                      tmp <- smooth.spline(.x, .y)
                      splinefun(tmp$x, tmp$y)
                    }),
            y = map2(list(.data$Frame), list(.data$Y), ~ 
                    {
                      tmp <- smooth.spline(.x, .y)
                      splinefun(tmp$x, tmp$y)
                    }),
            frames = map(list(.data$Frame), ~ unique(.x))) %>%
        ungroup() %>%
        
        mutate(
            # calculate velocity over time
            v_x = map2(.data$x, .data$frames, function(x, f) x(f, deriv = 1)),
            v_y = map2(.data$y, .data$frames, function(y, f) y(f, deriv = 1)),
            v = map2(.data$v_x, .data$v_y, ~ sqrt(.x^2 + .y^2) * sign(.y)), # going down = positive velocity, going up = negative velocity
            
            # convert functions of x and y back to values
            x = map2(.data$x, .data$frames, function(x, f) x(f)),
            y = map2(.data$y, .data$frames, function(y, f) y(f)),
            
            #directed vs undirected statistics for each channel
            directed_v_undirected = pmap(list(chan = .data$channel, frames = .data$frames, f = .data$x, g = .data$y),
                                         function(chan, frames, f, g)
                                             filter(dat_sub, .data$channel == chan) %>%
                                             dplyr::select(.data$Track, .data$Frame, .data$X, .data$Y) %>% 
                                             rename(f = .data$X, g = .data$Y) %>%
                                             compare_two_functions(frames, f, g, sig.figs)
                                         )
    
    # Proportion of cells in the channel that make it from top to bottom
    # Takes the sum of track_summ$finished for each channel and divides it by the corresponding length of track_sum$finished
    # Also adding channel averages for chemotactic efficiency and angle of migration
  
    # initializing empty vector which will be filled with proportions of cells completing path
    finished_by_channel <- c()
    ce_summ <- list()
    angle_summ <- list()
    max_v_summ <- list()
    
    # the for loop will iterate once through for each channel and filter by the data for that channel
    for (i in 1:length(channel_summ$channel)){
      temp <- filter(track_summ, channel == i) #filtering by the data for each channel in turn
      prop_finished <- sum(temp$finished) / length(temp$finished) #the proportion finished is equal to the sum of the 'finished' column in track_summ divided by the total entries in track_summ for that channel
      finished_by_channel <- append(finished_by_channel, prop_finished) #appending our calculated proportion to a vector. in the end this vector will contain all of the proportions finished for each channel
      
      #summary statistics for the chemotactic efficiency of the cells in each channel
      stats_ce <- list(min = min(temp$ce),first_q = quantile(temp$ce,0.25),median = median(temp$ce),third_q=quantile(temp$ce,0.75),max=max(temp$ce),mean=mean(temp$ce),range=range(temp$ce)[2]-range(temp$ce)[1])
      ce_summ[[i]] <- stats_ce
      
      #summary statistics for the angle of migration of the cells in each channel
      stats_angle <- list(min = min(temp$angle_migration),first_q = quantile(temp$angle_migration,0.25),median = median(temp$angle_migration),third_q=quantile(temp$angle_migration,0.75),max=max(temp$angle_migration),mean=mean(temp$angle_migration),range=(range(temp$angle_migration)[2]-range(temp$angle_migration)[1]))
      angle_summ[[i]] <- stats_angle
      
      stats_max_v <- list(min = min(temp$max_v),first_q = quantile(temp$max_v,0.25),median = median(temp$max_v),third_q=quantile(temp$max_v,0.75),max=max(temp$max_v),mean=mean(temp$max_v),range=range(temp$max_v)[2]-range(temp$max_v)[1])
      max_v_summ[[i]] <- stats_max_v
    }
    channel_summ <- channel_summ %>% mutate(ce_summ = ce_summ, angle_summ = angle_summ, finished = finished_by_channel, max_v_summ = max_v_summ)
  
    ##############################
    # Experiment-level summaries #
    ##############################
    
    exp_summ <- list()
    
    trts <- paste(unique(channel_summ$treatment))
    samps <- unique(channel_summ$sample)
    
    ### within_grp: Within-group statistics (within same sample, same treatment)
    within_grp <- group_by(dat_sub, .data$date, .data$experiment, .data$sample, .data$treatment) %>%
        
      # check to see how many channels we have in each group  
      mutate(nchannels = length(unique(.data$channel))) %>%

      # drop any groups that only have one channel (nothing to compare)
      dplyr::filter(.data$nchannels > 1)
    
    # if we have different within-group statistics to calculate...
    if(nrow(within_grp) > 0)
    {
      within_grp <- within_grp %>%
        
        # create one row per two-way comparison
        summarize(channel_a = combn(unique(.data$channel), 2)[1,],
                  channel_b = combn(unique(.data$channel), 2)[2,],
                  a_vs_b = list('')) %>%
        ungroup()
                                  
      # make comparisons for all pairs
      for(i in 1:nrow(within_grp))
      {
        # get functions to compare
        f <- filter(dat_sub, paste(.data$treatment) == paste(within_grp$treatment[i]) &
                             paste(.data$sample   ) == paste(within_grp$sample[i]) &
                                   .data$channel    == within_grp$channel_a[i])
        f <- list(smooth.spline(f$Frame, f$X),
                  smooth.spline(f$Frame, f$Y))
        
        g <- filter(dat_sub, paste(.data$treatment) == paste(within_grp$treatment[i]) &
                             paste(.data$sample   ) == paste(within_grp$sample[i]) &
                                   .data$channel    ==       within_grp$channel_b[i])
        g <- list(smooth.spline(g$Frame, g$X),
                  smooth.spline(g$Frame, g$Y))
      
        # calculate similarity and p-value
        within_grp$a_vs_b[[i]] <- filter(dat_sub, paste(.data$treatment) == paste(within_grp$treatment[i]) &
                                                  paste(.data$sample   ) == paste(within_grp$sample[i]) &
                                                        .data$channel   %in%    c(within_grp$channel_a[i], within_grp$channel_b[i])) %>%
          mutate(f = .data$X) %>%
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
        filter(.data$channel_a != -1)
    }
      
    exp_summ$within_grp <- within_grp
    

    ### btw_trt: Between-treatment statistics (same sample, different treatment)
    btw_trt <- group_by(dat_sub, .data$date, .data$experiment, .data$sample) %>%
      
      # check to see how many channels we have in each group  
      mutate(ntrts = length(unique(.data$treatment))) %>%
      
      # drop any groups that only have one channel (nothing to compare)
      dplyr::filter(.data$ntrts > 1)
    
    # if we have different between-treatment statistics to calculate...
    if(nrow(btw_trt) > 0)
    {
      btw_trt <- btw_trt %>%
        
        # create one row per two-way comparison
        summarize(trt_a = paste(combn(unique(.data$treatment), 2)[1,]),
                  trt_b = paste(combn(unique(.data$treatment), 2)[2,]),
                  a_vs_b = list('')) %>%
        ungroup()
      
      # make comparisons for all pairs
      for(i in 1:nrow(btw_trt))
      {
        # get functions to compare
        f <- filter(dat_sub, paste(.data$treatment) == paste(btw_trt$trt_a[i]) &
                                   .data$sample     ==       btw_trt$sample[i])
        f <- list(smooth.spline(f$Frame, f$X),
                  smooth.spline(f$Frame, f$Y))
        
        g <- filter(dat_sub, paste(.data$treatment) == paste(btw_trt$trt_b[i]) &
                                   .data$sample     ==       btw_trt$sample[i])
        g <- list(smooth.spline(g$Frame, g$X),
                  smooth.spline(g$Frame, g$Y))
        
        # calculate similarity and p-value
        btw_trt$a_vs_b[[i]] <- filter(dat_sub, paste(.data$treatment) %in% c(paste(btw_trt$trt_a[i]), paste(btw_trt$trt_b[i])) &
                                               sample == btw_trt$sample[i]) %>%
          mutate(f = .data$X) %>%
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
        filter(!is.na(.data$date))
    }

    exp_summ$btw_trt <- btw_trt
    
    
    ### btw_samp: Between-sample comparison statistics (same treatment, different sample)
    btw_samp <- group_by(dat_sub, .data$date, .data$experiment, .data$treatment) %>%
      
      # check to see how many channels we have in each group  
      mutate(nsamps = length(unique(.data$sample))) %>%
      
      # drop any groups that only have one channel (nothing to compare)
      dplyr::filter(.data$nsamps > 1)
    
    # if we have different between-treatment statistics to calculate...
    if(nrow(btw_samp) > 0)
    {
      btw_samp <- btw_samp %>%
        
        # create one row per two-way comparison
        summarize(a = paste(combn(unique(.data$sample), 2)[1,]),
                  b = paste(combn(unique(.data$sample), 2)[2,]),
                  a_vs_b = list('')) %>%
        ungroup()
      
      # make comparisons for all pairs
      for(i in 1:nrow(btw_samp))
      {
        # get functions to compare
        f <- filter(dat_sub, paste(.data$sample   ) == paste(btw_samp$a[i]) &
                             paste(.data$treatment) == paste(btw_samp$treatment[i]))
        f <- list(smooth.spline(f$Frame, f$X),
                  smooth.spline(f$Frame, f$Y))
          
        g <- filter(dat_sub, paste(.data$sample   ) == paste(btw_samp$b[i]) &
                             paste(.data$treatment) == paste(btw_samp$treatment[i]))
        g <- list(smooth.spline(g$Frame, g$X),
                  smooth.spline(g$Frame, g$Y))
        
        # calculate similarity and p-value
        btw_samp$a_vs_b[[i]] <- filter(dat_sub, paste(.data$sample  ) %in% c(paste(btw_samp$a[i]), paste(btw_samp$b[i])) &
                                                paste(.data$treatment) ==    paste(btw_samp$treatment[i])) %>%
          mutate(f = .data$X) %>%
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
                         a_vs_b = list('')) %>%
        filter(!is.na(.data$date))
    }
    
    exp_summ$btw_samp <- btw_samp
    

    ### tracks_time: time-coded tracks
    exp_summ$tracks_time <- arrange(dat_sub, .data$channel, .data$Track, .data$Frame) %>%
        
        mutate(lab = paste0(.data$channel, ": ", .data$sample, ", ", .data$treatment)) %>%
        
        ggplot(aes(x = .data$X, y = .data$Y, group = .data$Track, color = .data$Frame)) +
        geom_path() + #connects observations in the order in which they appear in the dataset
        
        ylab('Non-directed Movement') +
        xlab('Directed Movement') +
        
        facet_wrap(~ .data$lab) + #produces multi-panel plot, separate by 'lab'
        
        scale_y_reverse() +
        scale_color_gradient2(low  = 'blue',
                              mid  = rgb(  0, .62, .45),
                              high = rgb(.9, .62, 0),
                              midpoint = 60) +
        
        geom_hline(yintercept = 0, linetype = 2) + #draws a horizontal line at y = 0
        geom_hline(yintercept = 1, linetype = 2) #draws a horizontal line at y = 1
    

     
    ### tracks_v: velocity over time graphs
    exp_summ$tracks_v <-  pivot_longer(track_summ, starts_with('v'), names_to = 'd', values_to = 'v') %>%
      filter(.data$d != 'v') %>% #filters out first row (I think)
      #d = direction, pivot_longer makes data frame longer by making a column 'd' for direction (x-undirected, y-directed)
      
      # split out velocity curves for each track
      group_by(.data$channel, .data$Track, .data$sample, .data$treatment, .data$d) %>% #making data frame longer allows us to split by directed and undirected (d)
      summarize(v = unlist(.data$v), #list -> vector
                Frame = unlist(.data$frames)) %>%
      mutate(grp = paste(.data$channel, .data$d)) %>%
      ungroup()  %>% 
    
   
      # join channels that have the same sample and treatment
      group_by(sample, .data$treatment, .data$d) %>%
        mutate(joint_channels = paste0(paste(unique(.data$channel), collapse = '/'), ": ", unique(.data$sample), ', ', unique(.data$treatment))) %>%
        ungroup() %>%
        
        # sort and plot
        arrange(.data$channel, .data$d, .data$Track, .data$Frame) %>%
    
    
      #plots velocity (v) over time (Frame), grouping the data by grp which is channel and direction, coloring them by direction  
      ggplot(aes(.data$Frame, .data$v, group = .data$grp, color = .data$d)) +

      stat_smooth(method = lm, formula = y ~ bs(x, df = 3), se = FALSE) +
      stat_smooth(method = lm, formula = y ~ 1, se = FALSE, linetype = 2, size = .5) +
        
        #facet_grid(~ joint_channels) +
        
        scale_color_manual(values = c('black', 'gold3'), labels = c('Undirected', 'Directed')) +
        theme(legend.title = element_blank(),
              legend.position = 'top') +
        ylab('Relative Velocity') +
        geom_hline(yintercept = 0, linetype = 3, size = .5)
    
    #if sample and treatment are available, split by these labels; if not, use channels
    if(!(any(is.na(track_summ$sample)))){
      exp_summ$tracks_v <- exp_summ$tracks_v + facet_grid(~.data$joint_channels)
    }else{
      exp_summ$tracks_v <- exp_summ$tracks_v + facet_grid(~.data$channel)
    }
    ##### Save and Return Results #####
    
    save(track_summ, channel_summ, exp_summ, file = paste0(root, '/data/', unique(dat_sub$experiment), '.RData'))
}

    
    # Proportion of cells in the channel that make it from top to bottom
    # Takes the sum of track_summ$finished for each channel and divides it by the corresponding length of track_sum$finished
    # Also adding channel averages for chemotactic efficiency and angle of migration
  
    # initializing empty vector which will be filled with proportions of cells completing path
    finished_by_channel <- c()
    ce_summ <- list()
    angle_summ <- list()
    max_v_summ <- list()
    
    # the for loop will iterate once through for each channel and filter by the data for that channel
    for (i in 1:length(channel_summ$channel)){
      temp <- filter(track_summ, channel == i) #filtering by the data for each channel in turn
      prop_finished <- sum(temp$finished) / length(temp$finished) #the proportion finished is equal to the sum of the 'finished' column in track_summ divided by the total entries in track_summ for that channel
      finished_by_channel <- append(finished_by_channel, prop_finished) #appending our calculated proportion to a vector. in the end this vector will contain all of the proportions finished for each channel
      
      #summary statistics for the chemotactic efficiency of the cells in each channel
      stats_ce <- list(min = min(temp$ce),first_q = quantile(temp$ce,0.25),median = median(temp$ce),third_q=quantile(temp$ce,0.75),max=max(temp$ce),mean=mean(temp$ce),range=range(temp$ce)[2]-range(temp$ce)[1])
      ce_summ[[i]] <- stats_ce
      
      #summary statistics for the angle of migration of the cells in each channel
      stats_angle <- list(min = min(temp$angle_migration),first_q = quantile(temp$angle_migration,0.25),median = median(temp$angle_migration),third_q=quantile(temp$angle_migration,0.75),max=max(temp$angle_migration),mean=mean(temp$angle_migration),range=(range(temp$angle_migration)[2]-range(temp$angle_migration)[1]))
      angle_summ[[i]] <- stats_angle
      
      stats_max_v <- list(min = min(temp$max_v),first_q = quantile(temp$max_v,0.25),median = median(temp$max_v),third_q=quantile(temp$max_v,0.75),max=max(temp$max_v),mean=mean(temp$max_v),range=range(temp$max_v)[2]-range(temp$max_v)[1])
      max_v_summ[[i]] <- stats_max_v
    }
    channel_summ <- channel_summ %>% mutate(ce_summ = ce_summ, angle_summ = angle_summ, finished = finished_by_channel, max_v_summ = max_v_summ)
  
    
    ### angle of migration
    #track_summ <- mutate(track_summ,labs=paste0(channel, ": ", sample, ", ", treatment))
    exp_summ$angle_migration_plot <- track_summ %>% group_by(channel,sample,treatment) %>% ggplot(aes(x=as.character(channel),y=angle_migration,group=channel)) + geom_violin(scale = "width",width = 0.5,trim=FALSE) + ylab("Migration angle (degrees from vertical)") + geom_jitter(width = 0.1,size = 0.75,alpha=0.3) + geom_boxplot(width = 0.05) +
       xlab("Channel")
    
    ### chemotactic efficiency
    exp_summ$ce_plot <- track_summ %>% group_by(channel,sample,treatment) %>% ggplot(aes(x=as.character(channel),y=ce,group=channel)) + geom_violin(scale = "width",width = 0.5,trim=FALSE) + ylab("Chemotactic efficiency (% vertical movement)") + geom_jitter(width = 0.1,size = 0.75,alpha=0.3) + geom_boxplot(width = 0.05) +
       xlab("Channel")
    
    
    # table for percent finishing path
    exp_summ$finished_table <- channel_summ %>% select(channel,finished)
    names(exp_summ$finished_table) <- c("Channel","Proportion cells finishing path")
    
    # table for angle of migration summary statistics
    exp_summ$angle_table <- data.frame(data.frame(t(sapply(channel_summ$angle_summ,c))))
    exp_summ$angle_table <- cbind(channel_summ$channel,exp_summ$angle_table)
    names(exp_summ$angle_table) <- c("Channel","Min","First Quartile","Median","Third Quartile","Maximum","Mean","Range")
    
    # table for chemotactic efficiency
    exp_summ$ce_table <- data.frame(data.frame(t(sapply(channel_summ$ce_summ,c))))
    exp_summ$ce_table <- cbind(channel_summ$channel,exp_summ$ce_table)
    names(exp_summ$ce_table) <- c("Channel","Min","First Quartile","Median","Third Quartile","Maximum","Mean","Range")

    
    # ### tracks_v_stats: velocity statistics
    # nl_buffer_vs_nl_trt_x <- filter(dat_sub, sample == 'nl') %>%
    #     mutate(trt = ifelse(channel == 1, 'buffer', as.character(treatment))) %>%
    #     
         

    # ### Chemotactic efficiency
    # # (net vertical distance) / (total distance)
    # chemotactic_efficiency = (y_max - y_min) / sum(sqrt((X[-1] - X[-length(X)])^2 + 
    #                                                         (Y[-1] - Y[-length(Y)])^2)),
    # 
    # ### Angle and Magnitude of migration
    # tmp = abs(atan((X[1] - X[length(X)]) / (Y[1] - Y[length(Y)]))),
    # angle_of_migration = ifelse(Y[length(Y)] > Y[1],
    #                             tmp,
    #                             180 - tmp),
    # total_velocity = sqrt((X[1] - X[length(X)])^2 + (Y[1] - Y[length(Y)])^2) / len) %>%
    
            )