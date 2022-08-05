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
  # run through the loop once for every track
  # even though some files don't start at track 1, it is important to start the loop at track 1 to keep the indices consistent 
  for (i in 1:max(dat$Track)){
    # min_dist will be a vector of the minimum distance from this track to another track at each frame
    # it will contain NA for all frames where the selected Track is not measured
    min_dist <- c()
    # for each track, the loop will repeat once for each Frame
    # even though some tracks start at Frame 0, I need to start the loop at Frame 1 for consistent indexing
    for(j in 1:max(dat$Frame)){
      temp <- filter(dat, Frame == j)
      # if the selected Track is measured at this Frame, the minimum distance will be calculated and appended to min_dist
      if(i %in% temp$Track){
        track_i <- temp[temp$Track == i,]
        dist_i <- sqrt((track_i$X - temp$X)^2 + (track_i$Y - temp$Y)^2)
        dist_i <- dist_i[dist_i != 0]
        if (length(dist_i)!= 0){
          min_dist <- append(min_dist,min(dist_i))}
      }
      # if the selected Track is not measured at this frame, the minimum distance will be marked as NA
      # this is an important placeholder so the proper frames are recorded later on in the process
      else{
        min_dist <- append(min_dist,NA)
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
    if (!(is.na(cur_track[i]))){
      if (cur_track[i] > threshold){
        #if the distance is less than the threshold, appends the Frame to the list of good frames
        good_frames <- append(good_frames, i)
      } 
    }
  }
  return(good_frames)
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


