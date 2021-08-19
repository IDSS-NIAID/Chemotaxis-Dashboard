# one_experiment.R
# process data from one experiment and save
# this is called from historical_data.R (check there for package dependencies)

one_experiment <- function(dat_sub)
{
    # start summarization of tracks
    dat_sub <- group_by(dat_sub, f, date, experiment, channel, sample, treatment, Track) %>%
        mutate(
            # this is the frame where the cell first crosses the upper ledge
            cross_at = case_when(    Y[1] >= 0  ~ Frame[1],
                                     all(Y    <  0) ~ as.double(NA),
                                     TRUE           ~ suppressWarnings(min(Frame[c(FALSE, Y[-length(Y)] < 0 & Y[-1] >= 0)], na.rm = TRUE))),
        
            # X and Y are already scaled - translate X st each cell starts at (0,~0) when first crossing top ledge
            X = X - X[Frame == cross_at],
        
            # calculate velocity over time
            v_x = c(NA, (X[-1] - X[-length(X)]) / (Frame[-1] - Frame[-length(Frame)])),
            v_y = c(NA, (Y[-1] - Y[-length(Y)]) / (Frame[-1] - Frame[-length(Frame)])),
            v = sqrt(v_x^2 + v_y^2) * sign(v_y), # going down = positive velocity, going up = negative velocity
        
            # check that we have more than 1 observation
            l = sum(!is.na(X))) %>% 
        filter(l > 4)

    
    ##### channel summaries #####
    channel_summ <- summarize(dat_sub, len = sum(!is.na(X)),
                            y_min = min(Y, na.rm = TRUE),
                            y_max = max(Y, na.rm = TRUE),
                            
                            # Smoothed Velocity Curves
                            # this is a hack to get the entire smooth.spline object to be saved to the tibble for each track
                            smooth_v_y = map2(list(Frame[-1]), list(v_y[-1]), ~ smooth.spline(.x, .y, keep.data = FALSE)),
                            smooth_v_x = map2(list(Frame[-1]), list(v_x[-1]), ~ smooth.spline(.x, .y, keep.data = FALSE)),
                            smooth_x = map2(list(Frame), list(X), ~ smooth.spline(.x, .y, keep.data = FALSE)),
                            smooth_y = map2(list(Frame), list(Y), ~ smooth.spline(.x, .y, keep.data = FALSE))) %>%
                            
        # v_peak
        # v_post_peak
        # v_post_peak_acceleration) %>%
        ungroup() %>%
        
        # drop any "cells" that don't move at all (+/- a few pixels)
        filter(y_max - y_min > 0.01) %>%
    
        # drop anything that is missing smoothed curve
        filter(!is.na(smooth_v_y)) %>%
        group_by(f, date, channel, sample, treatment) %>%
        summarize(smooth_v_y = map2(list(unlist(sapply(smooth_v_y, `[`, 'x'))), # pull all Frames
                                    list(unlist(sapply(smooth_v_y, `[`, 'y'))), # pull all smoothed y velocities
                                    ~ smooth.spline(.x, .y, df = 7, keep.data = FALSE)),
                  smooth_v_x = map2(list(unlist(sapply(smooth_v_x, `[`, 'x'))), # pull all Frames
                                    list(unlist(sapply(smooth_v_x, `[`, 'y'))), # pull all smoothed y velocities
                                    ~ smooth.spline(.x, .y, df = 7, keep.data = FALSE))) %>%
        ungroup() %>%
        
        filter(is.na(treatment) | treatment != 'fMLF (did not work)') %>%
        
        # remove '.csv' from file names
        mutate(f = gsub('.csv', '', f, fixed = TRUE))
    
    
    ##### Experiment-level summaries #####
    exp_summ <- list()
    
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