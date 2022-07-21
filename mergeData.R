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
  system(intern = TRUE)

# stops executing if there are no files in the data file directory
if(length(data_files)==0){
  print("Please make sure there are files in the data directory.")
  stop("No files to process")
}

# saves an extra copy of the data files list, as the original will be edited
total_data_files <- data_files

# we only want the code to run on files that have not been incorporated into historical.RData
# therefore, if historical.RData exists, we want to filter out the files already contained within it
if("historical.RData" %in% data_files){
  load(paste0(root,'/', 'data/historical.RData'))
  # the following line is used to compare the data file names without extension to the experiments already loaded into historical.RData
  data_files <- tools::file_path_sans_ext(data_files) 
  data_files <- data_files[!(data_files == "historical")] #filter out historical.RData
  data_files <- data_files[!(data_files %in% track_summ_select$experiment)] #filter out all files already processed
}else{
  # the rest of the code assumes the files have been stripped of their extension
  data_files <- tools::file_path_sans_ext(data_files)
}

# MERGING DATA FILES #
# We load in the first data file in the list and save its data.frames
load(paste0(data_dir,"/",data_files[1],".RData"))
track_summ1 <- track_summ
channel_summ1 <- channel_summ
exp_summ1 <- exp_summ
# throughout the loop, we continue to append each file's data.frames to those of the first files (using rbind)
# the only files in data_files are those that are not included in historical.RData already
if(length(data_files) > 1){
  for(i in 2:length(data_files)){
    # load in each file in the directory in turn
    load(paste0(data_dir,"/",data_files[i],".RData"))
    # rbind this file's data.frames to the combined data.frames we are building
    track_summ1 <- rbind(track_summ1,track_summ)
    channel_summ1 <- rbind(channel_summ1,channel_summ)
    exp_summ1 <- rbind(exp_summ1,exp_summ)
  }
}

# if historical.RData exists, we want to rbind the data.frames we just made to those in historical.RData
if("historical.RData" %in% total_data_files){
  load(paste0(data_dir,"/historical.RData"))
  track_summ1 <- rbind(track_summ1,track_summ)
  channel_summ1 <- rbind(channel_summ1,channel_summ)
  exp_summ1 <- rbind(exp_summ1,exp_summ)
}

# renaming the data.frames to their original names
track_summ <- track_summ1
channel_summ <- channel_summ1
exp_summ <- exp_summ1

##### track_summ_select #####
# this significantly speeds up the selection module for track_summ
track_summ_select <- dplyr::select(channel_summ, date, experiment) %>%
  unique() %>%
  mutate(date = as.character(date)) # need date as character for subset widget in Shiny


# Save out file as "historical.RData" in the data directory
save(track_summ, channel_summ, exp_summ, track_summ_select, file = paste(data_dir, '/historical.RData', sep = '/'))



