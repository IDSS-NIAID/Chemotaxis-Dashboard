# simulated data for demo purposes
# Should run data-raw/process_19000101_data.R after this to update sample data provided with this package.

library(dplyr)
library(readr)

# root directory of the git repo
root <- system('git rev-parse --show-toplevel', intern = TRUE)
save_to <- file.path('inst', 'extdata')

dt <- '19000101'


# normal buffer - random walk
ntracks <- 100
nframes <- 120

set.seed(239487)
tibble(Track = rep(1:ntracks, each = nframes),
       Frame = rep(1:nframes, ntracks),
       X = rnorm(ntracks * nframes, mean = 0, sd = .01),
       Y = rnorm(ntracks * nframes, mean = 0, sd = .01)) %>%
  
  group_by(Track) %>%
  mutate(X = cumsum(X),
         Y = cumsum(Y)) %>%
  ungroup() %>%
  
  # yes, this is a 'csv' that is really tab delimited
  write_csv(file.path(root, save_to, paste0(dt, "_CH1_nl_Buffer.csv")))

# normal fML8 - directed in Y direction, random walk in X direction
ntracks <- 100
nframes <- 120

set.seed(23948)
tibble(Track = rep(1:ntracks, each = nframes),
       Frame = rep(1:nframes, ntracks),
       X = rnorm(ntracks * nframes, mean = 0, sd = .005),
       Y = rnorm(ntracks * nframes, mean = .01, sd = .005)) %>%
  
  group_by(Track) %>%
  mutate(X = cumsum(X),
         Y = cumsum(Y)) %>%
  ungroup() %>%
  
  # yes, this is a 'csv' that is really tab delimited
  write_csv(file.path(root, save_to, paste0(dt, "_CH2_nl_fMLF8.csv")))

set.seed(2394)
tibble(Track = rep(1:ntracks, each = nframes),
       Frame = rep(1:nframes, ntracks),
       X = rnorm(ntracks * nframes, mean = 0, sd = .005),
       Y = rnorm(ntracks * nframes, mean = .01, sd = .005)) %>%
  
  group_by(Track) %>%
  mutate(X = cumsum(X),
         Y = cumsum(Y)) %>%
  ungroup() %>%
  
  # yes, this is a 'csv' that is really tab delimited
  write_csv(file.path(root, save_to, paste0(dt, "_CH3_nl_fMLF8.csv")))

# case buffer - random walk (slower)
ntracks <- 50
nframes <- 120

set.seed(2394879)
tibble(Track = rep(1:ntracks, each = nframes),
       Frame = rep(1:nframes, ntracks),
       X = rnorm(ntracks * nframes, mean = 0, sd = .005),
       Y = rnorm(ntracks * nframes, mean = 0, sd = .005)) %>%
  
  group_by(Track) %>%
  mutate(X = cumsum(X),
         Y = cumsum(Y)) %>%
  ungroup() %>%
  
  # yes, this is a 'csv' that is really tab delimited
  write_csv(file.path(root, save_to, paste0(dt, "_CH4_c_Buffer.csv")))

# case fML8 - directed in Y direction, random walk in X direction
ntracks <- 50
nframes <- 120

set.seed(239489)
tibble(Track = rep(1:ntracks, each = nframes),
       Frame = rep(1:nframes, ntracks),
       X = rnorm(ntracks * nframes, mean = 0, sd = .0025),
       Y = rnorm(ntracks * nframes, mean = .01, sd = .0025)) %>%
  
  group_by(Track) %>%
  mutate(X = cumsum(X),
         Y = cumsum(Y)) %>%
  ungroup() %>%
  
  # yes, this is a 'csv' that is really tab delimited
  write_csv(file.path(root, save_to, paste0(dt, "_CH5_c_fMLF8.csv")))

set.seed(23949)
tibble(Track = rep(1:ntracks, each = nframes),
       Frame = rep(1:nframes, ntracks),
       X = rnorm(ntracks * nframes, mean = 0, sd = .0025),
       Y = rnorm(ntracks * nframes, mean = .01, sd = .0025)) %>%
  
  group_by(Track) %>%
  mutate(X = cumsum(X),
         Y = cumsum(Y)) %>%
  ungroup() %>%
  
  # yes, this is a 'csv' that is really tab delimited
  write_csv(file.path(root, save_to, paste0(dt, "_CH6_c_fMLF8.csv")))
