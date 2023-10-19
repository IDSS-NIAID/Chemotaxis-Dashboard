# pixel_to_um.R

library(dplyr)
library(readr)

root <- system('git rev-parse --show-toplevel', intern = TRUE)

# files to convert
files <- c('20170131_CH2_NL_fMLF.csv', '20170131_CH5_WW_fMLF.csv')

for(f in files)
{
  file.path(root, 'utils', 'results_csv', f) %>%
    read_delim(delim = '\t') %>%
    
    mutate(X = X*260,
           Y = Y*260) %>%
    
    write_csv(file.path(root, 'tmp', f))
}
