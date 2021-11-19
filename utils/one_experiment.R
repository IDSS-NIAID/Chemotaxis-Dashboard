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
#' @param nperms Number of permutations for permutation testing
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
compare_two_functions <- function(.data, frames.f, f, g, nperms, frames.g = frames.f, lab = NULL)
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
  
  perms <- replicate(nperms, 
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
    c(dissim = dissim,
      p = sum(dissim < perms) / nperms) %>%
        return()
}

#' Analysis of data for each experiment
#' 
#' @param dat_sub data frame (tibble) containing the subset of data for a single experiment
#' @param nperms number of permutations to use in permutation test statistics
#' 
#' @value A data frame containing channel-level summaries of the data in dat_sub. A list containing experiment-level statistics is also saved to an RData file.
one_experiment <- function(dat_sub, nperms = 10000)
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
            y = map2(y, frames, function(y, f) y(f)))
    
    
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
            
            # directed vs undirected statistics for each channel
            directed_v_undirected = pmap(list(chan = channel, frames = frames, f = x, g = y),
                                         function(chan, frames, f, g)
                                             filter(dat_sub, channel == chan) %>%
                                             dplyr::select(Track, Frame, X, Y) %>% 
                                             rename(f = X, g = Y) %>%
                                             compare_two_functions(frames, f, g, nperms)))


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
      dplyr::filter(nchannels > 1) %>%
        
      # create one row per two-way comparison (I think this will return an empty tibble when the input tibble is empty...)
      summarize(channel_a = combn(unique(channel), 2)[1,],
                channel_b = combn(unique(channel), 2)[2,],
                a_vs_b = list('')) %>%
      ungroup()
                                  
    # make comparisons for all pairs
    for(i in 1:nrow(within_grp))
    {
      # get functions to compare
      f <- filter(dat_sub, paste(treatment) == paste(within_grp$treatment[i]) &
                           sample == within_grp$sample[i] &
                           channel == within_grp$channel_a[i]) %>%
        with(list(smooth.spline(Frame, X),
                  smooth.spline(Frame, Y)))
      g <- filter(dat_sub, paste(treatment) == paste(within_grp$treatment[i]) &
                    sample == within_grp$sample[i] &
                    channel == within_grp$channel_b[i]) %>%
        with(list(smooth.spline(Frame, X),
                  smooth.spline(Frame, Y)))
      
      # calculate similarity and p-value
      within_grp$a_vs_b[[i]] <- filter(dat_sub, paste(treatment) == paste(within_grp$treatment[i]) &
                                         sample == within_grp$sample[i] &
                                         channel %in% c(within_grp$channel_a[i], within_grp$channel_b[i])) %>%
        mutate(f = X) %>%
        compare_two_functions(f = sapply(f, function(.x) .x$y), frames.f = f[[1]]$x, 
                              g = sapply(g, function(.x) .x$y), frames.g = g[[1]]$x,
                              nperms = 10, lab = 'channel')
    }
      

    ### between_trt: Between-treatment statistics

    # for each treatment/sample group
    ## calculate smoothed curves (including 1st derivative, velocity) by channel
    ## compare smoothed curves within each group (when more than one channel exists)
    
    # compare between treatment/sample groups
    ## calculate within-group smoothed curves (including 1st derivative, velocity) - summarizing across channels
    ## compare smoothed curves between groups
    
    # comparisons are done via permutation testing using similarity measures from fdakma::kma.similarity()
    
    for(i in trts)
    {
        # random vs directed comparison for each sample for treatment, i
        rand_vs_directed <- foreach(j = samps) %do%
        {
            tmp <- filter(dat_sub, paste(treatment) == i & paste(sample) == j & !is.na(X) & !is.na(Y)) %>%
                dplyr::select(Frame, Track, X, Y) %>%
                pivot_wider(names_from = Track, values_from = c(X, Y))

            fdaregre(tmp, c(sum(grepl('X', names(tmp))), sum(grepl('Y', names(tmp)))), c('Random', 'Directed'))
        }
    }
    

    ### tracks_time: time-coded tracks
    exp_summ$tracks_time <- arrange(dat_sub, channel, Track, Frame) %>%
        
        mutate(lab = paste0(channel, ": ", sample, ", ", treatment)) %>%
        
        ggplot(aes(x = X, y = Y, group = Track, color = Frame)) +
        geom_path() +
        
        ylab('Non-directed Movement') +
        xlab('Directed Movement') +
        
        facet_wrap(~ lab) +
        
        scale_y_reverse() +
        scale_color_gradient2(low  = 'blue',
                              mid  = rgb(  0, .62, .45),
                              high = rgb(.9, .62, 0),
                              midpoint = 60) +
        
        geom_hline(yintercept = 0, linetype = 2) +
        geom_hline(yintercept = 1, linetype = 2)
    
    ### tracks_v: velocity measures
    exp_summ$tracks_v <- pivot_longer(dat_sub, starts_with('v'), names_to = 'd', values_to = 'v') %>%
        mutate(joint_channels = case_when(channel  ==  1   ~ paste0('1: ', sample, ', ', treatment),
                                          channel %in% 2:3 ~ paste0('2/3: ', sample, ', ', treatment),
                                          channel  ==  4   ~ paste0('4: ', sample, ', ', treatment),
                                          TRUE             ~ paste0('5/6: ',  sample, ', ', treatment)),
               grp = paste(sample, treatment, d, channel)) %>%
        filter(d != 'v' & !is.na(v)) %>%
        arrange(channel, d, Track, Frame) %>%
        
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