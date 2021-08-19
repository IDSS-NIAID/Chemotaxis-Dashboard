# process historical data

# assumptions:
#   track inferences have been downloaded and extracted to the root/utils directory (e.g. https://abcs-amp.cancer.gov/uploads/internal/111/82a695a58ceea6537bf44203885d5916997111a5)

library(dplyr)
library(purrr)
library(readr)
library(tidyr)

library(lme4)
library(splines)

options(dplyr.summarise.inform = FALSE)

library(ggplot2)
library(cowplot)

theme_cowplot() %>%
    theme_set()

library(foreach)
library(doParallel)
registerDoParallel(cores = detectCores())

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

#########
# Utils #
#########

paste0(root, '/utils/one_experiment.R') %>%
    source()

###################
# historical data #
###################

# experiment metadata
results_meta <- tibble(
    f = results,
    
    dat = strsplit(f, '_', fixed = TRUE),
    
    date = {map_chr(dat, `[`, 1) %>% 
            substr(1, 8) %>% 
            as.Date(format = '%Y%m%d')},
    
    channel = map_int(dat, ~ 
                          {grep(.x, pattern = 'CH[1-6]', value = TRUE) %>%
                           substr(3, 3) %>%
                           as.integer()}),
    
    sample = map_chr(dat, ~
                       {.x[.x != ''][-1] %>%
                        gsub(pattern = '.csv', replacement = '', fixed = TRUE) %>%
                        grep(pattern = '^[0-9]$', invert = TRUE, value = TRUE) %>% # drop any single digit numbers
                        grep(pattern = 'CH[1-6]', invert = TRUE, value = TRUE) %>% # drop channel
                        grep(pattern = 'fMLF|Basal|Buffer|C5a|SDF|IL.|LTB4', ignore.case = TRUE, invert = TRUE, value = TRUE) %>% # drop attractant
                        grep(pattern = 'I8RA', invert = TRUE, value = TRUE)}[1]), # catch a typo
    
    treatment = map_chr(dat, ~ 
                      {.x %>%
                       gsub(pattern = '.csv', replacement = '', fixed = TRUE) %>%
                       grep(.x, pattern = 'fMLF|Basal|Buffer|C5a|SDF|IL.|LTB4|I8RA',
                            ignore.case = TRUE, value = TRUE)}[1])) %>%
    
    mutate(sample = tolower(sample), # inconsistent capitalization
           ledges = map_chr(f, ~ readLines(paste(results_dir, .x, sep = '/'), n = 1)), # top and bottom ledges
           topLedge = map_int(ledges, ~ as.integer(strsplit(.x, ',', fixed = TRUE)[[1]][3])),
           bottomLedge = map_int(ledges, ~ as.integer(strsplit(.x, ',', fixed = TRUE)[[1]][4]))) %>%
    
    select(-dat, -ledges)
    

##### read in data #####
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
           X = X / scale,
           
    # pull experiment name
           experiment = map_chr(f, ~ strsplit(.x, '_CH', fixed = TRUE)[[1]][1])) %>%
    
    filter(!is.na(X) & !is.na(Y))


##### track_summ_select #####
# this significantly speeds up the selection module for track_summ
track_summ_select <- select(dat, date, experiment) %>%
    unique() %>%
    mutate(date = as.character(date)) # need date as character for subset widget in Shiny


##### channel_summ #####
# Summary of each channel, including the following information:
## 

# Summary data from each experiment is also saved:
## Figures
### tracks_time
### tracks_v

## Tables of statistics
### velocity_stats

channel_summ <- foreach(current_experiment = track_summ_select$experiment[1:8], .combine = rbind) %dopar%
    {
        filter(dat, experiment == current_experiment) %>%
            one_experiment()
    }

save(channel_summ, track_summ_select, file = paste(root, 'data/historical.RData', sep = '/'))

# additional measures that may be of interest
# length of each track (total distance travelled)
# proportion of cells that made it from top to bottom
# velocity statistics (mean, sd)
# peak velocity
# post-peak velocity
# post-peak acceleration


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
