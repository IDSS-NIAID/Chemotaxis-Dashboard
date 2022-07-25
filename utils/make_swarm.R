#' make_swarm.R
#' Compare csv output from cell tracking model with preprocessed results and create a swarm file for preprocessing new data
#'
#' @param csvPath Path to csv files from cell tracking model (default is root/utils/results_csv)
#' @param datPath Path to preprocessed data (default is root/data)
#' @param root Path to git repository (default assumes this code is run from within the repo)
#' 
#' @examples 
#' Here is an example call using default data locations:
#' `Rscript make_swarm.R`
#' 
#' Here is another couple of examples, specifying alternate data locations:
#' `Rscript make_swarm.R --args csvPath=/data/IDSS_projects/chemotaxis_results/results_csv datPath=/data/IDSS_projects/chemotaxis_results/data`
#' `Rscript make_swarm.R --args root=~/Chemotaxis-Dashboard`

library(dplyr)
library(purrr)
library(tools)


################
# command args #
################

tmp <- commandArgs(TRUE) %>%
  strsplit('=', fixed = TRUE)

args <- map(tmp, ~ .x[2])
names(args) <- map_chr(tmp, ~ .x[1])

# defaults
if(!'root' %in% names(args))
  args$root <- system('git rev-parse --show-toplevel', intern = TRUE)

if(!'csvPath' %in% names(args))
  args$csvPath <- paste0(args$root, '/utils/results_csv')

if(!'datPath' %in% names(args))
  args$datPath <- paste0(args$root, '/data')


###################################
# compare files in each directory #
###################################

# all raw csv files
csv_files <- paste('ls', args$csvPath) %>%
  system(intern = TRUE)

# all processed .RData files
dat_files <- paste('ls', args$datPath) %>%
  system(intern = TRUE)

experiments <- file_path_sans_ext(dat_files)

# which experiments need processing?
toProcess <- sapply(csv_files, function(x){
  
  # are we missing an .RData file that matches this csv file?
  sapply(experiments, function(pattern){
    !any(grepl(pattern, x, fixed = TRUE))}) %>%
    
    # if all .RData files don't match this csv file, we are missing it
    all()
})
  
# get experiment names for `preprocess.R`
experimentsToProcess <- csv_files[toProcess] %>%
  
  file_path_sans_ext() %>%
  strsplit('_', fixed = TRUE) %>%
  sapply(`[`, 1) %>%
  unique()


####################
# write swarm file #
####################

paste0('module load R; cd ', args$root, '; Rscript utils/preprocess.R --args experiment=', experimentsToProcess, collapse = '\n') %>%
  cat(file = 'swarmfile')
