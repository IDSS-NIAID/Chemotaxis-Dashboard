# one_experiment.R
# process data from one experiment and save
# this is called from historical_data.R (check there for package dependencies)

#' function to display "Too much missing information" message in place of a regular figure
missing_info <- function()
{
    tibble(x = 1, y = 1, label = 'Too much missing information') %>%
        ggplot(aes(x = x, y = y, label = label)) +
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
#' @param .data A subset of dat_sub with variables, f and g, for permutation testing. When `lab` is specified, only `f` is used.
#' @param frames A vector of frames over which the function, f, is defined
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
#' @value A named vector containing the similarity measure and permutation p-value
#' .data <- filter(dat_sub, channel == 1) %>% dplyr::select(Track, Frame, X, Y) %>% rename(f = X, g = Y)
#' frames <- channel_summ$frames[[1]]
#' f <- channel_summ$x[[1]]
#' g <- channel_summ$y[[1]]
compare_two_functions <- function(.data, frames.f, f, g, sig.figs, frames.g = frames.f, lab = NULL)
{

  if(is.null(lab))
  {
    # tracks for permutation test
    tracks <- unique(.data$Track)
  }else{
    # if we are using a label, tabulate how many tracks fall into each group
    grps <- dplyr::select(.data, Track, matches(lab)) %>%
      unique() %>%
      dplyr::select(-Track) %>%
      unlist() %>%
      paste() %>% # need this to catch 'NA' as an option
      table()
      
    # labels for permutation test
    .data$perm_lab <- paste(.data[[lab]], .data$Track)
    tracks <- unique(.data$perm_lab)
    
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
  
        shuffled <- mutate(.data, 
                           f.tmp = ifelse(Track %in% tracks[pick_track], g, f),
                           g     = ifelse(Track %in% tracks[pick_track], f, g),
                           f     = f.tmp)
            
        # smooth f and g
        f <- with(shuffled, smooth.spline(Frame, f))
        g <- with(shuffled, smooth.spline(Frame, g))
      }else{
        # pick tracks to be in group 1, the others fall into group 2 (should exactly two groups)
        grp1 <- sample(tracks, grps[1])
          
        # shuffle
        shuffled <- mutate(.data,
                           lab = ifelse(perm_lab %in% grp1, names(grps)[1], names(grps)[2]))
                             
        # smooth f and g
        f <- dplyr::filter(shuffled, lab == names(grps)[1]) %>%   # filter by shuffled group
          with(list(smooth.spline(Frame, X),                      # smooth undirected function
                    smooth.spline(Frame, Y)))                     # smooth directed function
      
        f <- list(x = sapply(f, function(.x) .x$x),
                  y = sapply(f, function(.x) .x$y))               # return two columns (un/directed) of smoothed functions
        
        g <- dplyr::filter(shuffled, lab == names(grps)[2]) %>%   # filter by shuffled group
          with(list(smooth.spline(Frame, X),                      # smooth undirected function
                    smooth.spline(Frame, Y)))                     # smooth directed function
      
        g <- list(x = sapply(g, function(.x) .x$x),
                  y = sapply(g, function(.x) .x$y))               # return two columns (un/directed) of smoothed functions
      }
        
      # calculate similarity
      suppressWarnings(kma.similarity(x.f = f$x, y0.f = f$y, 
                                      x.g = g$x, y0.g = g$y,
                                      similarity.method = 'd0.L2'))
    })
    
    # return permutation test p-value, null hypothesis is that f and g are the same
    retval <- c(dissim = dissim,
                p = sum(dissim < perms) / 1*10^i)
    
    if(retval['p'] > 1*10^-(i - 1))
      break
  }
  
  return(retval)
}

#' Analysis of data for each experiment
#' 
#' @param dat_sub data frame (tibble) containing the subset of data for a single experiment
#' @param sig.figs maximum significant digits for p-values obtained by permutation testing
#' 
#' @value A data frame containing channel-level summaries of the data in dat_sub. A list containing experiment-level statistics is also saved to an RData file.
one_experiment <- function(dat_sub, sig.figs = 4)
{
    ##################################
    # Prep dat_sub for summarization #
    ##################################
    
    dat_sub <- group_by(dat_sub, f, date, experiment, channel, sample, treatment, Track) %>%
        mutate(
            # this is the frame where the cell first crosses the upper ledge
            cross_at = case_when(    Y[1] >= 0  ~ Frame[1],
                                     all(Y    <  0) ~ Frame[1], # if it never crosses the top ledge, use time 0 as the starting point
                                     TRUE           ~ suppressWarnings(min(Frame[c(FALSE, Y[-length(Y)] < 0 & Y[-1] >= 0)], na.rm = TRUE))),
        
            # X and Y are already scaled - translate X st each cell starts at (0,~0) when first crossing top ledge
            X = X - X[Frame == cross_at],
            
            y_min = min(Y),
            y_max = max(Y)) %>%
        ungroup() %>%
        
        # drop any "cells" that don't move at all (+/- a few pixels)
        filter(y_max - y_min > 0.01 &
               # drop this one that didn't work
               (is.na(treatment) | treatment != 'fMLF (did not work)')) %>%
        
        # remove '.csv' from file names
        mutate(f = gsub('.csv', '', f, fixed = TRUE))
    
    
    #############################
    # Track-level summarization #
    #############################
    
    track_summ <- group_by(dat_sub, channel, sample, treatment, Track) %>%
        
        # make sure we have enough observations to use the track
        mutate(l = sum(!is.na(X))) %>%
        filter(l > 3) %>%
        
        # calculate smooth functions of X and Y over time
        summarize(
            x = map2(list(Frame), list(X), ~ with(smooth.spline(.x, .y), splinefun(x, y))),
            y = map2(list(Frame), list(Y), ~ with(smooth.spline(.x, .y), splinefun(x, y))),
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
            
            # Chemotactic efficiency
            # calculate total change in y direction - return a value for each track
            delta_y = map_dbl(y, ~diff(range(.x))),
            # calculate the total distance travelled using the distance formula
            distance_travelled = map2_dbl(x,y,~sum( sqrt( (.x[-1]-.x[-length(.x)])^2+(.y[-1]-.y[-length(.y)])^2 ) )),
            # chemotactic efficiency for each track is the change in Y (delta_y) divided by the total distance (distance_travelled) 
            ce = delta_y / distance_travelled,
            
            # Angle of migration
            # We need the total change in x direction to calculate the angle of migration
            delta_x = map_dbl(x, ~diff(range(.x))),
            #this finds the angle of migration between the start and end point, in radians
            angle_migration = abs(atan(delta_x/delta_y)),
            
            # Proportion of cells making it past the threshold
            # at the track level, we want to know if the cell ever passes the y-position 1
            # to understand that, we set a variable "finished" to be 1 if the cell crosses the threshold and 0 if it does not
            max_y = map_dbl(y, ~max(.x)), 
            finished = ifelse(max_y >= 1, 1, 0)
            
            ) 
    
    #I am not sure where in the code this fits best but the proportion of cells crossing the threshold is equal to the sum of finished divided by the length of finished
    #prop_finished <- sum(track_summ$finished)/length(track_summ$finished)
   
    
    ###############################
    # Channel-level summarization #
    ###############################
    
    channel_summ <- group_by(dat_sub, f, date, experiment, channel, sample, treatment) %>%
        
        # calculate smooth functions of X and Y over time
        summarize(
            x = map2(list(Frame), list(X), ~ with(smooth.spline(.x, .y), splinefun(x, y))),
            y = map2(list(Frame), list(Y), ~ with(smooth.spline(.x, .y), splinefun(x, y))),
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
                                             dplyr::select(Track, Frame, X, Y) %>% 
                                             rename(f = X, g = Y) %>%
                                             compare_two_functions(frames, f, g, sig.figs)
                                         ),
            
            )
    
    # Proportion of cells in the channel that make it from top to bottom
    # Takes the sum of track_summ$finished for each channel and divides it by the corresponding length of track_sum$finished
    # I ended up resorting to writing a for loop for this, even though I know it isn't very efficient
    # I wasn't sure what I could do to access both channel_summ and track_summ, as I think both are needed for this calculation
    
    # initializing empty vector which will be filled with proportions of cells completing path
    finished_by_channel <- c()
    # the for loop will iterate once through for each channel and filter by the data for that channel
    for (i in 1:length(channel_summ$channel)){
      temp <- filter(track_summ, channel == i) #filtering by the data for each channel in turn
      prop_finished <- sum(temp$finished) / length(temp$finished) #the proportion finished is equal to the sum of the 'finished' column in track_summ divided by the total entries in track_summ for that channel
      finished_by_channel <- append(finished_by_channel, prop_finished) #appending our calculated proportion to a vector. in the end this vector will contain all of the proportions finished for each channel
    }
    channel_summ <- cbind(channel_summ,finished_by_channel) #binding the proportion finished for each channel to the channel_summ dataframe
  #channel_summ$finished_by_channel #testing
  
    ##############################
    # Experiment-level summaries #
    ##############################
    
    exp_summ <- list()
    
    trts <- paste(unique(channel_summ$treatment))
    samps <- unique(channel_summ$sample)
    
    ### within_grp: Within-group statistics (within same sample, same treatment)
    within_grp <- group_by(dat_sub, date, experiment, sample, treatment) %>%
        
      # check to see how many channels we have in each group  
      mutate(nchannels = length(unique(channel))) %>%

      # drop any groups that only have one channel (nothing to compare)
      dplyr::filter(nchannels > 1)
    
    # if we have different within-group statistics to calculate...
    if(nrow(within_grp) > 0)
    {
      within_grp <- within_grp %>%
        
        # create one row per two-way comparison
        summarize(channel_a = combn(unique(channel), 2)[1,],
                  channel_b = combn(unique(channel), 2)[2,],
                  a_vs_b = list('')) %>%
        ungroup()
                                  
      # make comparisons for all pairs
      for(i in 1:nrow(within_grp))
      {
        # get functions to compare
        f <- filter(dat_sub, paste(treatment) == paste(within_grp$treatment[i]) &
                             paste(sample) == paste(within_grp$sample[i]) &
                             channel == within_grp$channel_a[i]) %>%
          with(list(smooth.spline(Frame, X),
                    smooth.spline(Frame, Y)))
        g <- filter(dat_sub, paste(treatment) == paste(within_grp$treatment[i]) &
                             paste(sample) == paste(within_grp$sample[i]) &
                             channel == within_grp$channel_b[i]) %>%
          with(list(smooth.spline(Frame, X),
                    smooth.spline(Frame, Y)))
      
        # calculate similarity and p-value
        within_grp$a_vs_b[[i]] <- filter(dat_sub, paste(treatment) == paste(within_grp$treatment[i]) &
                                                  paste(sample) == paste(within_grp$sample[i]) &
                                                  channel %in% c(within_grp$channel_a[i], within_grp$channel_b[i])) %>%
          mutate(f = X) %>%
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
      mutate(ntrts = length(unique(treatment))) %>%
      
      # drop any groups that only have one channel (nothing to compare)
      dplyr::filter(ntrts > 1)
    
    # if we have different between-treatment statistics to calculate...
    if(nrow(btw_trt) > 0)
    {
      btw_trt <- btw_trt %>%
        
        # create one row per two-way comparison
        summarize(trt_a = paste(combn(unique(treatment), 2)[1,]),
                  trt_b = paste(combn(unique(treatment), 2)[2,]),
                  a_vs_b = list('')) %>%
        ungroup()
      
      # make comparisons for all pairs
      for(i in 1:nrow(btw_trt))
      {
        # get functions to compare
        f <- filter(dat_sub, paste(treatment) == paste(btw_trt$trt_a[i]) &
                      sample == btw_trt$sample[i]) %>%
          with(list(smooth.spline(Frame, X),
                    smooth.spline(Frame, Y)))
        g <- filter(dat_sub, paste(treatment) == paste(btw_trt$trt_b[i]) &
                      sample == btw_trt$sample[i]) %>%
          with(list(smooth.spline(Frame, X),
                    smooth.spline(Frame, Y)))
        
        # calculate similarity and p-value
        btw_trt$a_vs_b[[i]] <- filter(dat_sub, paste(treatment) %in% c(paste(btw_trt$trt_a[i]), paste(btw_trt$trt_b[i])) &
                                               sample == btw_trt$sample[i]) %>%
          mutate(f = X) %>%
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
      mutate(nsamps = length(unique(sample))) %>%
      
      # drop any groups that only have one channel (nothing to compare)
      dplyr::filter(nsamps > 1)
    
    # if we have different between-treatment statistics to calculate...
    if(nrow(btw_samp) > 0)
    {
      btw_samp <- btw_samp %>%
        
        # create one row per two-way comparison
        summarize(a = paste(combn(unique(sample), 2)[1,]),
                  b = paste(combn(unique(sample), 2)[2,]),
                  a_vs_b = list('')) %>%
        ungroup()
      
      # make comparisons for all pairs
      for(i in 1:nrow(btw_samp))
      {
        # get functions to compare
        f <- filter(dat_sub, paste(sample) == paste(btw_samp$a[i]) &
                      paste(treatment) == paste(btw_samp$treatment[i])) %>%
          with(list(smooth.spline(Frame, X),
                    smooth.spline(Frame, Y)))
        g <- filter(dat_sub, paste(sample) == paste(btw_samp$b[i]) &
                      paste(treatment) == paste(btw_samp$treatment[i])) %>%
          with(list(smooth.spline(Frame, X),
                    smooth.spline(Frame, Y)))
        
        # calculate similarity and p-value
        btw_samp$a_vs_b[[i]] <- filter(dat_sub, paste(sample) %in% c(paste(btw_samp$a[i]), paste(btw_samp$b[i])) &
                                                paste(treatment) == paste(btw_samp$treatment[i])) %>%
          mutate(f = X) %>%
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
        filter(!is.na(date))
    }
    
    exp_summ$btw_samp <- btw_samp
    

    ### tracks_time: time-coded tracks
    exp_summ$tracks_time <- arrange(dat_sub, channel, Track, Frame) %>%
        
        mutate(lab = paste0(channel, ": ", sample, ", ", treatment)) %>%
        
        ggplot(aes(x = X, y = Y, group = Track, color = Frame)) +
        geom_path() + #connects observations in the order in which they appear in the dataset
        
        ylab('Non-directed Movement') +
        xlab('Directed Movement') +
        
        facet_wrap(~ lab) + #produces multi-panel plot, separate by 'lab'
        
        scale_y_reverse() +
        scale_color_gradient2(low  = 'blue',
                              mid  = rgb(  0, .62, .45),
                              high = rgb(.9, .62, 0),
                              midpoint = 60) +
        
        geom_hline(yintercept = 0, linetype = 2) + #draws a horizontal line at y = 0
        geom_hline(yintercept = 1, linetype = 2) #draws a horizontal line at y = 1
    
    ### tracks_v: velocity measures
    exp_summ$tracks_v <- pivot_longer(track_summ, starts_with('v'), names_to = 'd', values_to = 'v') %>%
      filter(d != 'v') %>% #filters out first row (I think)
      #d = direction, pivot_longer makes data frame longer by making a column 'd' for direction (x-undirected, y-directed)
      
      # split out velocity curves for each track
      group_by(channel, Track, sample, treatment, d) %>% #making data frame longer allows us to split by directed and undirected (d)
      summarize(v = unlist(v), #list -> vector
                Frame = unlist(frames)) %>%
      mutate(grp = paste(channel, d)) %>%
      ungroup() %>%
    
      # join channels that have the same sample and treatment
      group_by(sample, treatment, d) %>%
      mutate(joint_channels = paste0(paste(unique(channel), collapse = '/'), ": ", unique(sample), ', ', unique(treatment))) %>%
      ungroup() %>%

      # sort and plot
      arrange(channel, d, Track, Frame) %>%
      
      #plots velocity (v) over time (Frame), grouping the data by grp which is channel and direction, coloring them by direction  
      ggplot(aes(Frame, v, group = grp, color = d)) +

      stat_smooth(method = lm, formula = y ~ bs(x, df = 3), se = FALSE) +
      stat_smooth(method = lm, formula = y ~ 1, se = FALSE, linetype = 2, size = .5) +
        
        facet_grid(~ joint_channels) +
        
        scale_color_manual(values = c('black', 'gold3'), labels = c('Undirected', 'Directed')) +
        theme(legend.title = element_blank(),
              legend.position = 'top') +
        ylab('Relative Velocity') +
        geom_hline(yintercept = 0, linetype = 3, size = .5)
    
    # ### tracks_v_stats: velocity statistics
    # nl_buffer_vs_nl_trt_x <- filter(dat_sub, sample == 'nl') %>%
    #     mutate(trt = ifelse(channel == 1, 'buffer', as.character(treatment))) %>%
    #     
    #     lmer(formula = 

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
    
    
    ##### Save and Return Results #####
    
    save(exp_summ, file = paste0('data/', unique(dat_sub$experiment), '.RData'))
    
    return(channel_summ)
}