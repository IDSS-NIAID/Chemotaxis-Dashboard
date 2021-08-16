# process historical data

# assumptions:
#   track inferences have been downloaded and extracted to the root/utils directory (e.g. https://abcs-amp.cancer.gov/uploads/internal/111/82a695a58ceea6537bf44203885d5916997111a5)

library(dplyr)
library(purrr)
library(readr)

options(dplyr.summarise.inform = FALSE)

library(ggplot2)
library(cowplot)

theme_cowplot() %>%
    theme_set()

#########
# Paths #
#########

# root directory of the git repo
root <- system('git rev-parse --show-toplevel', intern = TRUE)

# all updated results can be found here
results_dir <- paste(root, 'utils', 'result_csv', sep = '/')

# all results files
results <- paste('ls', results_dir) %>%
    system(intern = TRUE)

###################
# historical data #
###################

# experiment metadata
results_meta <- tibble(
    f = results,
    
    dat = strsplit(f, '_', fixed = TRUE),
    
    dt = {map_chr(dat, `[`, 1) %>% 
          substr(1, 8) %>% 
          as.Date(format = '%Y%m%d')},
    
    channel = map_int(dat, ~ 
                          {grep(.x, pattern = 'CH[1-6]', value = TRUE) %>%
                           substr(3, 3) %>%
                           as.integer()}),
    
    samp = map_chr(dat, ~
                       {.x[.x != ''][-1] %>%
                        gsub(pattern = '.csv', replacement = '', fixed = TRUE) %>%
                        grep(pattern = '^[0-9]$', invert = TRUE, value = TRUE) %>% # drop any single digit numbers
                        grep(pattern = 'CH[1-6]', invert = TRUE, value = TRUE) %>% # drop channel
                        grep(pattern = 'fMLF|Basal|Buffer|C5a|SDF|IL.|LTB4', ignore.case = TRUE, invert = TRUE, value = TRUE) %>% # drop attractant
                        grep(pattern = 'I8RA', invert = TRUE, value = TRUE)}[1]), # catch a typo
    
    trt = map_chr(dat, ~ 
                      {.x %>%
                       gsub(pattern = '.csv', replacement = '', fixed = TRUE) %>%
                       grep(.x, pattern = 'fMLF|Basal|Buffer|C5a|SDF|IL.|LTB4|I8RA',
                            ignore.case = TRUE, value = TRUE)}[1])) %>%
    
    mutate(samp = tolower(samp), # inconsistent capitalization
           ledges = map_chr(f, ~ readLines(paste(results_dir, .x, sep = '/'), n = 1)), # top and bottom ledges
           topLedge = map_int(ledges, ~ as.integer(strsplit(.x, ',', fixed = TRUE)[[1]][3])),
           bottomLedge = map_int(ledges, ~ as.integer(strsplit(.x, ',', fixed = TRUE)[[1]][4]))) %>%
    
    select(-dat, -ledges)
    

# read in data
dat <- map2_df(results_dir, results, ~ 
    {
        paste(.x, .y, sep = '/') %>%
        read_csv(col_types = 'dddd', skip = 1) %>%
        mutate(f = .y)
    }) %>%
    
    bind_rows() %>%
    
    left_join(results_meta, by = 'f') %>%
    
    # convert Y coordinates to [0,1] scale where 0 is the top ledge and 1 is the bottom ledge
    # negative values will be above the top ledge and values greater than 1 are below the bottom ledge
    # X values will be on the same scale (i.e. traveling .1 in the X direction is the same number of pixels as .1 in the Y direction)
    mutate(scale = bottomLedge - topLedge,
           Y = (Y - topLedge) / scale,
           X = X / scale)# %>%
    
    #select(-scale)
        
# for debugging
# a = "20051108___CH1_nl.csv"
# b = 1
# c = 'nl'
# d = NA
# e = 2

# summary of each track
track_summ <- filter(dat, !is.na(X) & !is.na(Y)) %>% 
    #filter(f == a, channel == b, samp == c, is.na(trt), Track == e) %>% # for debugging
    group_by(f, dt, channel, samp, trt, Track) %>%
    mutate(
        # this is the frame where the cell first crosses the upper ledge
        cross_at = case_when(    Y[1] >= 0  ~ Frame[1],
                             all(Y    <  0) ~ as.double(NA),
                             TRUE           ~ suppressWarnings(min(Frame[c(FALSE, Y[-length(Y)] < 0 & Y[-1] >= 0)], na.rm = TRUE))),
        
        # X and Y are already scaled - translate X st each cell starts at (0,~0) when first crossing top ledge
        X = X - X[Frame == cross_at],
            
        v_x = c(NA, (X[-1] - X[-length(X)]) / (Frame[-1] - Frame[-length(Frame)])),
        v_y = c(NA, (Y[-1] - Y[-length(Y)]) / (Frame[-1] - Frame[-length(Frame)])),
        v = sqrt(v_x^2 + v_y^2) * sign(v_y), # going down = positive velocity, going up = negative velocity
           
    # check that we have more than 1 observation
           l = sum(!is.na(X))) %>% 
    filter(l > 4) %>%
    
    # calculate velocity in different directions and smooth       
    summarize(len = sum(!is.na(X)),
              y_min = min(Y, na.rm = TRUE),
              y_max = max(Y, na.rm = TRUE),
              
              # this is a hack to get the entire smooth.spline object to be saved to the tibble for each track
              smooth_v_y = map2(list(Frame[-1]), list(v_y[-1]), ~ smooth.spline(.x, .y, keep.data = FALSE)),
              smooth_v_x = map2(list(Frame[-1]), list(v_x[-1]), ~ smooth.spline(.x, .y, keep.data = FALSE)),
              smooth_x = map2(list(Frame), list(X), ~ smooth.spline(.x, .y, keep.data = FALSE)),
              smooth_y = map2(list(Frame), list(Y), ~ smooth.spline(.x, .y, keep.data = FALSE)),
              
              # Chemotactic efficiency (net vertical distance) / (total distance)
              chemotactic_efficiency = (y_max - y_min) / sum(sqrt((X[-1] - X[-length(X)])^2 + 
                                                                  (Y[-1] - Y[-length(Y)])^2)),
              
              # Angle and Magnitude of migration
              tmp = abs(atan((X[1] - X[length(X)]) / (Y[1] - Y[length(Y)]))),
              angle_of_migration = ifelse(Y[length(Y)] > Y[1],
                                          tmp,
                                          180 - tmp),
              total_velocity = sqrt((X[1] - X[length(X)])^2 + (Y[1] - Y[length(Y)])^2) / len) %>%
           # v_peak
           # v_post_peak
           # v_post_peak_acceleration) %>%
    ungroup() %>%
    
    # drop any "cells" that don't move at all (+/- a few pixels)
    filter(y_max - y_min > 0.01)

# summary of each channel
channel_summ <- filter(track_summ, !is.na(smooth_v_y)) %>%
    group_by(f, dt, channel, samp, trt) %>%
    summarize(smooth_v_y = map2(list(unlist(sapply(smooth_v_y, `[`, 'x'))), # pull all Frames
                                list(unlist(sapply(smooth_v_y, `[`, 'y'))), # pull all smoothed y velocities
                                ~ smooth.spline(.x, .y, df = 7, keep.data = FALSE)),
              smooth_v_x = map2(list(unlist(sapply(smooth_v_x, `[`, 'x'))), # pull all Frames
                                list(unlist(sapply(smooth_v_x, `[`, 'y'))), # pull all smoothed y velocities
                                ~ smooth.spline(.x, .y, df = 7, keep.data = FALSE))) %>%
    ungroup() %>%
    
    filter(is.na(trt) | trt != 'fMLF (did not work)') %>%
    
    # remove '.csv' from file names
    mutate(f = gsub('.csv', '', f, fixed = TRUE))

# go ahead and drop some of this stuff that we don't need
track_summ <- select(track_summ, -smooth_v_y, -smooth_v_x)

# additional measures that may be of interest
# length of each track
# proportion of cells that made it from top to bottom
# velocity statistics (mean, sd)
# peak velocity
# post-peak velocity
# post-peak acceleration

channel_summ <- rename(channel_summ, 
                       sample = samp,
                       treatment = trt,
                       date = dt) %>%
    mutate(experiment = map_chr(f, ~ strsplit(.x, '_CH', fixed = TRUE)[[1]][1]),
           date = as.character(date)) # this is required to get dropdown search to work

track_summ <- rename(track_summ,
                     sample= samp,
                     treatment = trt,
                     date = dt) %>%
    mutate(experiment = map_chr(f, ~ strsplit(.x, '_CH', fixed = TRUE)[[1]][1]))

# this significantly speeds up the selection module for track_summ
track_summ_select <- select(track_summ, date, experiment) %>%
    unique()


save(channel_summ, track_summ_select, file = paste(root, 'data/historical.RData', sep = '/'))

# save individual track data (if not already present)
ind_tracks <- paste0('ls ', root, '/data') %>%
    system(intern = TRUE)

for(i in track_summ_select$experiment)
{
    f <- paste0(root, '/data/', i, '.RData')
    
    if(!any(grepl(f, ind_tracks)))
    {
        dat <- subset(track_summ, experiment == i)
        save(dat, file = f)
    }
}

######################
# normal buffer data #
######################

# extract all normal controls run in buffer
nml <- filter(channel_summ, sample == 'nl' & !is.na(treatment)) %>%
    select(smooth_v_y, experiment, channel, treatment) %>%
    pmap_df(function(smooth_v_y, experiment, channel, treatment){
        tibble(experiment = experiment,
               channel = channel,
               treatment = treatment,
               Frame = smooth_v_y$x,
               v_y = smooth_v_y$y)
    })

# look at velocity vs time over all normal samples (by treatment)
pdf(paste(root, 'y_velocity.pdf', sep = '/'))
mutate(nml,
       grp = paste(experiment, channel, sep = '_')) %>%
    ggplot(aes(Frame, v_y, group = grp)) +
    geom_line(color = rgb(0,0,0,.3)) +
    
    geom_hline(yintercept = 0, linetype = 2, size = 0.2, color = rgb(0,0,1)) +
    
    facet_wrap(~ treatment) +

    ylab('Realitve Velocity (y)') +
    theme(axis.text.x = element_text(size = 6))
dev.off()
