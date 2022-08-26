#make sure to call this before using these functions
library(dplyr)
library(tidyverse)

#############
# Functions #
#############

#' FILTER BEFORE STARTING LINE
#' 
#' This function finds all frames where the selected track is located before the starting line and
#' returns a vector of these frames where the track is 'ineligible'.
#' This is useful because before the starting line, there are many untracked cells which overlap the tracked cells,
#' making gathering accurate shape data difficult.
#' This function will be called if the user inputs TRUE for the parameter to_filter in find_frames
#' @param dat the data.frame where the position over time data is stored. Should have Track, Frame, X, and Y columns
#' @param starting_line the Y position of the starting line on the chemotaxis images. The function will find frames with Y position before this point 
#' @param track_num the selected track
#' @return a vector containing the frames for which the Y position of the selected track is before the starting line
#' @example filter_start(dat_sub,100,12)
filter_start <- function(dat, starting_line, track_num){
  newdat <- filter(dat, Track == track_num, Y < starting_line)
  return(newdat$Frame)
}

#' CALCULATING MINIMUM DISTANCES
#' 
#' This function calculates the minimum distance to another track for each track and each frame in the dataset.
#' It returns a two-dimensional list which stores all the minimum distances for each track.
#' @param dat the data.frame where the position over time data is stored. Should have Track, Frame, X, and Y columns
#' @return A two-dimensional list which has an entry for each track. The entry for each track is a vector of the minimum distance to the nearest neighbor of that cell at each frame
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

#' THRESHOLD BY FRAME
#' 
#' This function returns a list of frames where the selected track's closest neighbor is outside the threshold distance.
#' @param listname a two-dimensional list containing a vector of minimum distances for each track- can be generated using all_min_dist()
#' @param threshold the distance between cells considered acceptable. Current threshold is 35 pixels, though it may need to be adjusted
#' @param track_num the selected track
#' @return A list of the 'good frames' for the selected track, at which the nearest neighboring cell is outside the threshold distance
#' @example thresholding_by_frame(all_min_dist(dat),35,12)
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

#' FINDING ALL ELIGIBLE FRAMES
#' This function creates a csv file of all frames where each track does not get too close to another track.
#' If the user wishes to eliminate frames before the selected 'starting line', they should set toFilter to TRUE and choose a starting line.
#' The function will then remove these frames from the list of eligible frames for each track.
#' This function calls all_min_dist, thresholding_by_frame, and filter_start.
#' @param dat the data.frame where the position over time data is stored. Should have Track, Frame, X, and Y columns.
#' @param threshold the distance between cells considered acceptable. Current threshold is 35 pixels, though it may need to be adjusted
#' @param toFilter boolean value set based on if the user wants to filter out cells before the starting line
#' @param starting_line the Y position of the starting line
#' @return a csv file of the 'good frames' for each track in the data file
find_frames <- function(dat,threshold,toFilter = FALSE,starting_line = NULL){
  theList <- all_min_dist(dat) #getting all min distances
  for (i in 1:length(theList)){
    t <- thresholding_by_frame(theList,threshold,i)
    if(toFilter){
      f <- filter_start(dat,starting_line,i) #finds frames before starting line
      temp <- intersect(f,t) #finding frames that are outside threshold but before starting line
      t <- t[!(t %in% temp)] #removing the before-start frames
    }
    labelName <- tools::file_path_sans_ext(dat$f[1])
    cat(paste(t,collapse=","),"\n",file = paste("good_frames/",labelName,"_goodFrames.csv",sep=""),append = TRUE)
  }
}

## FIND AND ADJUST THRESHOLD USING THESE FUNCTIONS
# The following two functions are only really used to find and adjust the pixel threshold, notes on how to use in the README

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
