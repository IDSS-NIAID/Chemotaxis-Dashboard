# process historical data

# assumptions:
#   track inferences have been downloaded and extracted to the root/raw directory (e.g. https://abcs-amp.cancer.gov/uploads/external/178/78ca162546b0f38ad63b453cd016569ded405482)

# library(RSQLite)

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
    dat = strsplit(results, '_', fixed = TRUE),
    dt = map_chr(dat, ~ 
                     {substr(.x[1], 1, 8) %>%
                      as.Date(format = '%Y%m%d')}),
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
                            ignore.case = TRUE, value = TRUE)}[1]))


# all channels start with CH[1:6]
if(FALSE)
    table(substr(unlist(channel_dirs), 1, 3))

# read in data
dat <- pmap(list(rep(exp_dirs, each = 6), unlist(channel_dirs), data_dirs),
            function(experiment, channel, data_dir){
                # look for files (just go with pre-filtered data for now
                f <- gsub(pattern = ' ', replacement = '\\ ', data_dir, fixed = TRUE) %>%
                    gsub(pattern = '(', replacement = '\\(', fixed = TRUE) %>%
                    gsub(pattern = ')', replacement = '\\)', fixed = TRUE) %>%
                    map_chr(~ paste('ls', .x, '| grep pre.csv')) %>%
                    system(intern = TRUE) #%>%
                    #gsub(pattern = '_pre', replacement = '', fixed = TRUE)

                # extract meta-data from channel
                channel <- strsplit(channel, '_', fixed = TRUE)[[1]]
                
                channel_num <- as.integer(substr(channel[1], 3, 3))
                
                if(length(channel) >= 2)
                {
                    samp <- tolower(channel[2])
                }else{
                    samp <- NA
                }
                
                if(length(channel) >= 3)
                {
                    trt <- tolower(channel[3])
                }else{
                    trt <- NA
                }
                
                
                # some are missing tracks
                if(length(f) == 1)
                {
                    dat <- read_csv(paste(data_dir, f, sep = '/'), col_types = 'dddd') %>%
                        mutate(experiment = experiment,
                               channel = channel_num,
                               sample = samp,
                               treatment = trt) %>%
                        select(experiment, channel, sample, treatment, Track, Frame, X, Y)
                }else{
                    dat <- tibble(experiment = experiment,
                                  channel = channel_num,
                                  sample = samp,
                                  treatment = trt,
                                  Track = as.numeric(NA),
                                  Frame = as.numeric(NA),
                                  X = as.numeric(NA),
                                  Y = as.numeric(NA))
                }
                
                return(dat)
            }) %>%
    bind_rows()

# summary of each track
track_summ <- filter(dat, !is.na(X) & !is.na(Y)) %>% 
    group_by(experiment, channel, sample, treatment, Track) %>%
    mutate(v_x = c(NA, (X[-1] - X[-length(X)]) / (Frame[-1] - Frame[-length(Frame)])),
           v_y = c(NA, (Y[-1] - Y[-length(Y)]) / (Frame[-1] - Frame[-length(Frame)])),
           v = sqrt(v_x^2 + v_y^2) * sign(v_y)) %>% # going down = positive velocity, going up = negative velocity
           
    summarize(len = sum(!is.na(X)),
              y_min = min(Y, na.rm = TRUE),
              y_max = max(Y, na.rm = TRUE),
              
              # this is a hack to get the entire smooth.spline object to be saved to the tibble for each track
              smooth_v_y = map2(list(Frame[-1]), list(v_y[-1]), ~ smooth.spline(.x, .y, keep.data = FALSE)),
              smooth_v_x = map2(list(Frame[-1]), list(v_x[-1]), ~ smooth.spline(.x, .y, keep.data = FALSE)),
              smooth_x = map2(list(Frame), list(X - X[1]), ~ smooth.spline(.x, .y, keep.data = FALSE)),
              smooth_y = map2(list(Frame), list(Y - Y[1]), ~ smooth.spline(.x, .y, keep.data = FALSE)),
              
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
    filter(y_max - y_min > 3)

# summary of each channel
channel_summ <- filter(track_summ, !is.na(smooth_v_y)) %>%
    group_by(experiment, channel, sample, treatment) %>%
    summarize(smooth_v_y = map2(list(unlist(sapply(smooth_v_y, `[`, 'x'))), # pull all Frames
                                list(unlist(sapply(smooth_v_y, `[`, 'y'))), # pull all smoothed y velocities
                                ~ smooth.spline(.x, .y, df = 7, keep.data = FALSE)),
              smooth_v_x = map2(list(unlist(sapply(smooth_v_x, `[`, 'x'))), # pull all Frames
                                list(unlist(sapply(smooth_v_x, `[`, 'y'))), # pull all smoothed y velocities
                                ~ smooth.spline(.x, .y, df = 7, keep.data = FALSE))) %>%
    ungroup() %>%
    
    filter(is.na(treatment) | treatment != 'fmlf (did not work)')

# go ahead and drop some of this stuff that we don't need
track_summ <- select(track_summ, -smooth_v_y, -smooth_v_x)

# additional measures that may be of interest
# length of each track
# proportion of cells that made it from top to bottom
# velocity statistics (mean, sd)
# peak velocity
# post-peak velocity
# post-peak acceleration

save(channel_summ, file = paste(root, 'historical.RData', sep = '/'))

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
