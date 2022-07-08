#make sure to call this before using these functions
library(dplyr)
library(tidyverse)
#############
# Functions #
#############

# FILTER BEFORE STARTING LINE
# this function finds all frames where the selected track is located before the starting line
# returns a vector of these frames where the track is 'ineligible'
filter_start <- function(dat, starting_line, track_num){
  newdat <- filter(dat, Track == track_num, Y < starting_line)
  return(newdat$Frame)
}

# CALCULATING MINIMUM DISTANCE
# This function calculates the minimum distance to another track for each track and each frame in the dataset
# It returns a two-dimensional list which stores all the minimum distances
all_min_dist <- function(dat){
  all_dist_list <- list()
  for (i in min(dat$Track):max(dat$Track)){
    min_dist <- c()
    for(j in min(dat$Frame):max(dat$Frame)){
      temp <- filter(dat, Frame == j)
      if(i %in% temp$Track){
        track_i <- temp[temp$Track == i,]
        dist_i <- sqrt((track_i$X - temp$X)^2 + (track_i$Y - temp$Y)^2)
        dist_i <- dist_i[dist_i != 0]
        if (length(dist_i)!= 0){
          min_dist <- append(min_dist,min(dist_i)) }
      }
    }
    all_dist_list[[i]] <- min_dist
  }
  return(all_dist_list)
}

# THRESHOLD BY FRAME
# Returns a list of frames where the selected track's closest neighbor is within the threshold distance
# Can be used to have more flexibility when selecting tracks, can eliminate only bad frames instead of whole track
thresholding_by_frame <- function(listname,threshold,track_num){
  cur_track <- listname[[track_num]]
  bad_frame <- c()
  for (i in 1:length(cur_track)){
    if (cur_track[i] < threshold){
      #if the distance is less than the threshold, appends the Frame to the list of bad frames
      bad_frame <- append(bad_frame, i)
    } 
  }
  return(bad_frame)
}

# THRESHOLD BY TRACK
# Returns TRUE if all distances to another track are above the threshold
# Returns false if another track is too close at any point
# Leads to a more conservative sample of non-overlapping tracks
thresholding_by_track <- function(listname,threshold,track_num){
  cur_track <- listname[[track_num]] 
  #if any distance in the list is below the threshold, it returns FALSE
  if(min(cur_track) < threshold){ 
    return(FALSE)
  }
  return(TRUE)
}

# FINDING ALL ELIGIBLE TRACKS
# should return a vector of all eligible tracks based on a threshold
# only returns tracks that are never within a certain distance (determined by threshold) of another cell
# this calls the stricter thresholding_by_track, so if any values are below the threshold, the whole track is thrown out
find_tracks <- function(dat,threshold){
  theList <- all_min_dist(dat) #getting all min distances
  tracks_vec <- c()
  for (i in 1:length(theList)){
    t <- thresholding_by_track(theList,threshold,i)
    if(t){
      tracks_vec <- append(tracks_vec,i) #only appends eligible tracks
    }
  }
  return(tracks_vec)
}

# FINDING ALL INELIGIBLE FRAMES
# returns a list of all ineligible frames for each track based on a threshold
# returns frames where the track gets too close to another track
# gives more flexibility in choosing tracks to examine
# if the user wishes to eliminate frames before the selected 'starting line', they should set toFilter to TRUE and choose a starting line
# the function will then append these frames to the list of ineligible frames for each track
find_frames <- function(dat,threshold,toFilter = FALSE,starting_line = NULL){
  theList <- all_min_dist(dat) #getting all min distances
  frames_list <- list()
  for (i in 1:length(theList)){
    t <- thresholding_by_frame(theList,threshold,i)
    if(toFilter){
      f <- filter_start(dat,starting_line,i) #finds frames before starting line
      temp <- intersect(f,t) #finding frames both within threshold and before starting line
      f <- f[!(f %in% temp)]
      t <- sort(append(f,t)) 
    }
    frames_list[[i]] <- t
  }
  return(frames_list)
}

# MAKE CSV OF BAD FRAMES
# turns list of 'bad frames' for each track into one csv per file
# toFilter is automatically set to FALSE unless otherwise noted
# if the user wants to filter out frames before the starting line, they shoudl set toFilter to TRUE and add a starting line
# this function calls find_frames and writes the output list to a csv
make_csv <- function(dat,threshold,labelName,toFilter = FALSE,starting_line = NULL){
  theList <- find_frames(dat,threshold,toFilter,starting_line)
  len <- max(lengths(theList))
  myDf <- t(map_dfc(theList, function(l) {
    c(l, rep(0, len - length(l))) #fills empty slots with 0 (can also be set to NA) in order to make the list rectangular
  }))
  write.csv(myDf, paste("bad_frames/",labelName,"_badFrames.csv"))
}
  
###########
# Testing #
###########

#filename <- "trackResults/20180215_CH2_NL_fMLF/20180215_CH2_NL_fMLF.csv"
filename <- "trackResults/20180215_CH1_NL_Basal/20180215_CH1_NL_Basal.csv"
labelName <- tools::file_path_sans_ext(basename(filename))
dat <- read.delim(filename, sep = ",")
threshold <- 35
make_csv(dat,threshold,labelName,TRUE,100)

find_frames(dat,threshold,TRUE,100)
filter_start(dat,100,12)

# QUESTIONS:
# Is there a more efficient / easier way to store the data than csv?
# Is there a way to save the data without adding a lot of zeros or NA? is 0 or NA preferred?
# (look up sparse matrix)
# Should we work to vectorize some of these functions since there are a lot of for loops?
