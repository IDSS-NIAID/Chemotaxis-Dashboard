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
# Returns a list of frames where the selected track's closest neighbor is outside the threshold distance
# Can be used to have more flexibility when selecting tracks, can eliminate only bad frames instead of whole track
thresholding_by_frame <- function(listname,threshold,track_num){
  cur_track <- listname[[track_num]]
  good_frames <- c()
  for (i in 1:length(cur_track)){
    if (cur_track[i] > threshold){
      #if the distance is less than the threshold, appends the Frame to the list of bad frames
      good_frames <- append(good_frames, i)
    } 
  }
  return(good_frames)
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

# FINDING ALL ELIGIBLE FRAMES
# creates a csv file of all frames where each track does not get too close to another track
# gives more flexibility in choosing tracks to examine
# if the user wishes to eliminate frames before the selected 'starting line', they should set toFilter to TRUE and choose a starting line
# the function will then remove these frames from the list of eligible frames for each track
find_frames <- function(dat,threshold,toFilter = FALSE,starting_line = NULL){
  theList <- all_min_dist(dat) #getting all min distances
  for (i in 1:length(theList)){
    t <- thresholding_by_frame(theList,threshold,i)
    if(toFilter){
      f <- filter_start(dat,starting_line,i) #finds frames before starting line
      temp <- intersect(f,t) #finding frames that are outside threshold but before starting line
      t <- t[!(t %in% temp)] #removing the before-start frames
    }
    labelName <- paste0(dat$experiment[1],"_CH",dat$channel[1])
    cat(paste(t,collapse=","),"\n",file = paste("good_frames/",labelName,"_goodFrames.csv",sep=""),append = TRUE)
  }
}

###########
# Testing #
###########

filename <- "trackResults/20171106__CH6_BQ_fMLF/20171106__CH6_BQ_fMLF.csv"
labelName <- tools::file_path_sans_ext(basename(filename))
dat <- read.delim(filename, sep = ",")
threshold <- 35
find_frames(dat,threshold,TRUE,100)

# TESTING IF FILE READS IN OK
labelName <- "20171106__CH6_BQ_fMLF"
filename2 <- paste("good_frames/",labelName,"_goodFrames.csv",sep="")
dat2 <- readLines(filename2)

