library(dplyr)
library(purrr)
library(readr)
library(tidyr)

library(fdakma)

options(dplyr.summarise.inform = FALSE)

# Read in command args
tmp <- commandArgs(TRUE) %>%
  strsplit('=', fixed = TRUE)

# Reading in file to automate track selection
# for debugging: args <- list(experiment='20180215')
args <- map(tmp, ~ .x[2])
names(args) <- map_chr(tmp, ~ .x[1])

root <- system('git rev-parse --show-toplevel', intern = TRUE)

# all updated results can be found here
#results_dir <- paste(root, 'results_csv', sep = '/') #these are adjusted results

# results still in pixels are here (on my Biowulf -- will need to adjust for others)
results_dir <- paste(root, 'trackResults_2',sep = '/')

# all results files
results <- paste('ls', results_dir) %>%
  system(intern = TRUE) %>%
  grep(pattern = args$experiment, value = TRUE)


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
# CHECK DELIM WITH FILES YOU ARE PROCESSING
# files in results_csv are tab delimited while files in trackResults (and trackResults_2) are comma delimited
# make sure to adjust delim in the code below accordingly
dat <- map2_df(results_dir, results, ~ 
                 {
                   paste(.x, .y, sep = '/') %>%
                     read_delim(delim = ',', col_types = 'dddd') %>%
                     mutate(f = .y)
                 }) %>%
  
  bind_rows() %>%
  
  left_join(results_meta, by = 'f') %>%
  
  # pull experiment name
  mutate(experiment = map_chr(f, ~ (strsplit(.x, '_CH', fixed = TRUE)[[1]][1]) %>%
                                gsub(pattern = '_', replacement = '', fixed = TRUE))) %>%
  
  filter(!is.na(X) & !is.na(Y))

filenames <- unique(tools::file_path_sans_ext(dat$f))
for (i in 1:length(filenames)){
  paste0("python3 ", root, "/utils/shapeAnalysis1.py ",  filenames[i]) %>% system()
}
