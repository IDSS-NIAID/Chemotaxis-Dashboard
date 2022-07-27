######################
# Merge .RData files #
######################

# Run this script after running preprocess.R on however many experiments you are interested in
# This script will merge any new files in Chemotaxis-Dashboard/data into historical.RData, so that they can be used in the Shiny App

# make sure these packages are loaded
library(dplyr)

# this finds the working directory, so regardless of the user's file organization, the code should work
root <- system('git rev-parse --show-toplevel', intern = TRUE)

# this is finding the data file directory ("Chemotaxis-Dashboard/data")
# this is the directory where the processed .RData files live
# if the data files are located somewhere else, edit this code to direct there
data_dir <- paste(root, 'data', sep = '/')

# reads in all data files in the data file directory
data_files <- paste('ls', data_dir) %>%
  system(intern = TRUE) %>%
  grep(pattern = 'historical', invert = TRUE, value = TRUE) %>% # drop historical.RData
  grep(pattern = '19000101', invert = TRUE, value = TRUE) # don't include simulated data

# experiment IDs
experiments <- tools::file_path_sans_ext(data_files)

# stops executing if there are no files in the data file directory
if(length(data_files)==0){
  print("Please make sure there are files in the data directory.")
  stop("No files to process")
}

# we only want the code to run on files that have not been incorporated into historical.RData
# therefore, if historical.RData exists, we want to filter out the files already contained within it
if(file.exists(paste0(root, "/data/historical.RData"))){
  load(paste0(root, '/data/historical.RData'))
  all <- list(channel_summ = channel_summ,
              track_summ = track_summ)
}else{
  all <- list(channel_summ = NULL,
              track_summ = NULL)
}


# MERGING DATA FILES #

# throughout the loop, we continue to append each file's data.frames to those of the first files (using rbind)
# the only files in data_files are those that are not included in historical.RData already
for(i in experiments[!experiments %in% all$channel_summ$experiment]){
  # load in each file in the directory in turn
  load(paste0(data_dir,"/", i, ".RData"))
  
  # rbind this file's data.frames to the combined data.frames we are building
  all$track_summ   <- rbind(all$track_summ,    track_summ)
  all$channel_summ <- rbind(all$channel_summ1, channel_summ)
}


##### track_summ_select #####
# this significantly speeds up the selection module for track_summ
track_summ_select <- dplyr::select(channel_summ, date, experiment) %>%
  unique() %>%
  mutate(date = as.character(date)) # need date as character for subset widget in Shiny


# Save out file as "historical.RData" in the data directory
save(all, track_summ_select, file = paste(data_dir, '/historical.RData', sep = '/'))
