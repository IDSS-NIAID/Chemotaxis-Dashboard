#' preprocess.R
#' This will read in results from the convolutional model and preprocess for the dashboard
#' 
#' @param experiment Date of the experiment to run. Argument should be of the form: experiment=%Y%m%d.
#' @param root Location of the root of this repository (default is inferred from `git rev-parse`)
#' @param results_dir Location of raw csv tracks files
#' @param data_dir Location of data to pass to one_experiment()
#' @param seed Random seed to use
#' @param sig.figs Number of significant figures to pass to one_experiment
#' 
#' @examples 
#' Here is an example line for the swarm file:
#' `module load R; Rscript ~/Chemotaxis-Dashboard/utils/preprocess.R experiment=20070308`


library(dplyr)
library(purrr)
library(readr)

library(ChemotaxisDashboard)
# library(tidyr)

options(dplyr.summarise.inform = FALSE)

library(cowplot)
library(ggplot2)

theme_cowplot() %>%
  theme_set()


##################
# Paths/Defaults #
##################

# Read in command args
tmp <- commandArgs(TRUE) %>%
  strsplit('=', fixed = TRUE)

# for debugging: args <- list(experiment='20070308')
# for debugging: args <- list(experiment='19000101')
args <- map(tmp, ~ .x[2])
names(args) <- map_chr(tmp, ~ .x[1])

# root directory of the git repo
if(is.null(args$root))
  args$root <- system('git rev-parse --show-toplevel', intern = TRUE)

# directory for the processed data
if(is.null(args$data_dir))
  args$dat_dir <- paste0(args$root, '/data/')

# set random seed if supplied
if(!is.null(args$seed))
  set.seed(as.numeric(args$seed))

# default sig.figs
if(is.null(args$sig.figs))
  args$sig.figs <- 4

# all updated results can be found here
if(is.null(args$results_dir))
  args$results_dir <- paste(args$root, 'utils', 'results_csv', sep = '/')

# all results files
results <- paste('ls', args$results_dir) %>%
  system(intern = TRUE) %>%
  grep(pattern = args$experiment, value = TRUE)


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
dat <- map2_df(args$results_dir, results, ~ 
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
one_experiment(dat, root = args$root, sig.figs = args$sig.figs, data_dir = args$data_dir)
