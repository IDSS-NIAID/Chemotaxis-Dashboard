#' preprocess.R
#' This will read in results from the convolutional model and preprocess for the dashboard
#' 
#' @param experiment Date of the experiment to run. Argument should be of the form: experiment=%Y%m%d.
#' 
#' @examples 
#' Here is an example line for the swarm file:
#' `module load R; Rscript ~/Chemotaxis-Dashboard/utils/preprocess.R experiment=20070308`


library(dplyr)
library(purrr)
library(readr)
library(tidyr)

library(fdakma)

options(dplyr.summarise.inform = FALSE)

library(ggplot2)
library(cowplot)

theme_cowplot() %>%
  theme_set()


#########
# Paths #
#########

# Read in command args
tmp <- commandArgs(TRUE) %>%
  strsplit('=', fixed = TRUE)

args <- map(tmp, ~ .x[2])
names(args) <- map_chr(tmp, ~ .x[1])

# root directory of the git repo
root <- system('git rev-parse --show-toplevel', intern = TRUE)

# all updated results can be found here
results_dir <- paste(root, 'utils', 'results_csv', sep = '/')

# all results files
results <- paste('ls', results_dir) %>%
  system(intern = TRUE) %>%
  grep(pattern = args$experiment, value = TRUE)


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
  
  mutate(sample = tolower(sample)) %>% # inconsistent capitalization
  
  dplyr::select(-dat)


##### read in data #####
dat <- map2_df(results_dir, results, ~ 
                 {
                   paste(.x, .y, sep = '/') %>%
                     read_delim(delim = '\t', col_types = 'dddd') %>%
                     mutate(f = .y)
                 }) %>%
  
  bind_rows() %>%
  
  left_join(results_meta, by = 'f') %>%
  
  # pull experiment name
  mutate(experiment = map_chr(f, ~ (strsplit(.x, '_CH', fixed = TRUE)[[1]][1]) %>%
                                   gsub(pattern = '_', replacement = '', fixed = TRUE))) %>%
  
  filter(!is.na(X) & !is.na(Y))

# pre-process data for this experiment
one_experiment(dat, root = root, sig.figs = 2)
